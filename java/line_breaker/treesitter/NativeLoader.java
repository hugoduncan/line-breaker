package line_sitter.treesitter;

import io.github.treesitter.jtreesitter.NativeLibraryLookup;

import java.io.IOException;
import java.io.InputStream;
import java.lang.foreign.Arena;
import java.lang.foreign.SymbolLookup;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

/**
 * ServiceLoader implementation for loading tree-sitter native libraries.
 * Loads both libtree-sitter (core) and libtree-sitter-clojure (grammar)
 * from classpath resources (native/os-arch/).
 *
 * The libraries are extracted to a shared temp directory and loaded in
 * dependency order: core library first, then language grammar.
 */
public class NativeLoader implements NativeLibraryLookup {

  private static Path extractedDir = null;
  private static Path treeSitterLib = null;
  private static Path clojureGrammarLib = null;

  @Override
  public SymbolLookup get(Arena arena) {
    try {
      extractLibraries();
      // Load core tree-sitter first
      SymbolLookup coreLookup = SymbolLookup.libraryLookup(treeSitterLib, arena);
      // Load clojure grammar second
      SymbolLookup grammarLookup = SymbolLookup.libraryLookup(clojureGrammarLib, arena);
      // Return combined lookup: grammar first (for tree_sitter_clojure symbol),
      // falling back to core for other symbols
      return name -> grammarLookup.find(name).or(() -> coreLookup.find(name));
    } catch (IOException e) {
      throw new RuntimeException("Failed to load tree-sitter libraries", e);
    }
  }

  /**
   * Get the path to the extracted clojure grammar library.
   * Must be called after get() has been invoked at least once.
   * Returns null if libraries have not been extracted yet.
   */
  public static Path getClojureGrammarPath() {
    return clojureGrammarLib;
  }

  private synchronized void extractLibraries() throws IOException {
    if (extractedDir != null && Files.exists(extractedDir)) {
      return;
    }

    String os = detectOs();
    String arch = detectArch();
    String coreLibName = coreLibraryName(os);
    String grammarLibName = grammarLibraryName(os);
    String resourceDir = "native/" + os + "-" + arch + "/";

    // Create shared temp directory
    extractedDir = Files.createTempDirectory("line-sitter-ts-");

    // Extract core tree-sitter library
    treeSitterLib = extractResource(resourceDir + coreLibName, extractedDir, coreLibName);

    // Extract clojure grammar library
    clojureGrammarLib = extractResource(resourceDir + grammarLibName, extractedDir, grammarLibName);

    // Schedule cleanup on JVM exit (LIFO order: directory last)
    extractedDir.toFile().deleteOnExit();
    treeSitterLib.toFile().deleteOnExit();
    clojureGrammarLib.toFile().deleteOnExit();
  }

  private Path extractResource(String resourcePath, Path targetDir, String fileName)
      throws IOException {
    InputStream is = getClass().getClassLoader().getResourceAsStream(resourcePath);
    if (is == null) {
      // Try java.library.path as fallback
      Path libPathResult = findInLibraryPath(fileName);
      if (libPathResult != null) {
        return libPathResult;
      }
      throw new IOException(
          "Could not find native library: " + fileName +
              " (searched: classpath:" + resourcePath + ", java.library.path)");
    }

    try (is) {
      Path target = targetDir.resolve(fileName);
      Files.copy(is, target, StandardCopyOption.REPLACE_EXISTING);
      return target;
    }
  }

  private Path findInLibraryPath(String libName) {
    String libPath = System.getProperty("java.library.path");
    if (libPath != null) {
      for (String dir : libPath.split(System.getProperty("path.separator"))) {
        Path candidate = Path.of(dir, libName);
        if (Files.exists(candidate)) {
          return candidate;
        }
      }
    }
    return null;
  }

  private String detectOs() {
    String osName = System.getProperty("os.name").toLowerCase();
    if (osName.contains("mac")) {
      return "darwin";
    } else if (osName.contains("linux")) {
      return "linux";
    }
    throw new UnsupportedOperationException("Unsupported OS: " + osName);
  }

  private String detectArch() {
    String arch = System.getProperty("os.arch");
    return switch (arch) {
      case "aarch64", "arm64" -> "aarch64";
      case "amd64", "x86_64" -> "x86_64";
      default -> throw new UnsupportedOperationException(
          "Unsupported architecture: " + arch);
    };
  }

  private String coreLibraryName(String os) {
    return switch (os) {
      case "darwin" -> "libtree-sitter.dylib";
      case "linux" -> "libtree-sitter.so";
      default -> throw new UnsupportedOperationException(
          "Unsupported OS: " + os);
    };
  }

  private String grammarLibraryName(String os) {
    return switch (os) {
      case "darwin" -> "libtree-sitter-clojure.dylib";
      case "linux" -> "libtree-sitter-clojure.so";
      default -> throw new UnsupportedOperationException(
          "Unsupported OS: " + os);
    };
  }
}
