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
 * ServiceLoader implementation for loading tree-sitter native library.
 * Loads from classpath resources (native/os-arch/libtree-sitter.dylib|so).
 */
public class NativeLoader implements NativeLibraryLookup {

  private static Path extractedLibrary = null;

  @Override
  public SymbolLookup get(Arena arena) {
    try {
      Path libPath = findLibrary();
      return SymbolLookup.libraryLookup(libPath, arena);
    } catch (IOException e) {
      throw new RuntimeException("Failed to load tree-sitter library", e);
    }
  }

  private synchronized Path findLibrary() throws IOException {
    if (extractedLibrary != null && Files.exists(extractedLibrary)) {
      return extractedLibrary;
    }

    String os = detectOs();
    String arch = detectArch();
    String libName = libraryName(os);
    String resourcePath = "native/" + os + "-" + arch + "/" + libName;

    // Try classpath resource first
    InputStream is = getClass().getClassLoader().getResourceAsStream(resourcePath);
    if (is != null) {
      try (is) {
        Path tempDir = Files.createTempDirectory("line-sitter-ts-");
        extractedLibrary = tempDir.resolve(libName);
        Files.copy(is, extractedLibrary, StandardCopyOption.REPLACE_EXISTING);
        return extractedLibrary;
      }
    }

    // Fall back to java.library.path
    String libPath = System.getProperty("java.library.path");
    if (libPath != null) {
      for (String dir : libPath.split(System.getProperty("path.separator"))) {
        Path candidate = Path.of(dir, libName);
        if (Files.exists(candidate)) {
          return candidate;
        }
      }
    }

    throw new IOException(
      "Could not find tree-sitter library: " + libName +
        " (searched: classpath:" + resourcePath + ", java.library.path)");
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

  private String libraryName(String os) {
    return switch (os) {
      case "darwin" -> "libtree-sitter.dylib";
      case "linux" -> "libtree-sitter.so";
      default -> throw new UnsupportedOperationException(
        "Unsupported OS: " + os);
    };
  }
}
