# Test script to check fungal trait packages
cat("=== Testing Fungal Trait Packages ===\n\n")

# Check currently installed packages
cat("1. Checking installed packages...\n")
installed_pkgs <- installed.packages()
fungal_pkgs <- c("fungaltraits", "funfun", "funguildr")

for (pkg in fungal_pkgs) {
  if (pkg %in% rownames(installed_pkgs)) {
    cat("✅ ", pkg, " is installed (v", installed_pkgs[pkg, "Version"], ")\n")
  } else {
    cat("❌ ", pkg, " is not installed\n")
  }
}

cat("\n2. Checking available packages on CRAN...\n")
available_pkgs <- available.packages()

for (pkg in fungal_pkgs) {
  if (pkg %in% rownames(available_pkgs)) {
    cat("✅ ", pkg, " is available on CRAN\n")
    cat("   Description: ", strwrap(available_pkgs[pkg, "Title"], width=60)[1], "...\n")
  } else {
    cat("❌ ", pkg, " is not available on CRAN\n")
  }
}

cat("\n3. Testing fungaltraits if available...\n")
if ("fungaltraits" %in% rownames(installed_pkgs)) {
  tryCatch({
    library(fungaltraits)
    cat("✅ fungaltraits loaded successfully\n")

    # Check if it has mycorrhizal data
    if (exists("fungal_traits")) {
      cat("✅ fungal_traits dataset found\n")
      mycorrhizal_cols <- grep("mycorrh", names(fungal_traits), ignore.case=TRUE)
      if (length(mycorrhizal_cols) > 0) {
        cat("✅ Found mycorrhizal columns: ", paste(names(fungal_traits)[mycorrhizal_cols], collapse=", "), "\n")
        cat("   Sample values: ", paste(unique(fungal_traits[[names(fungal_traits)[mycorrhizal_cols[1]]]]), collapse=", "), "\n")
      }
    }

    # Check available functions
    cat("   Available functions: ", paste(grep("^ft_", ls("package:fungaltraits"), value=TRUE), collapse=", "), "\n")

  }, error = function(e) {
    cat("❌ Error loading fungaltraits: ", e$message, "\n")
  })
} else {
  cat("❌ fungaltraits not installed, skipping test\n")
}

cat("\n4. Testing funfun if available...\n")
if ("funfun" %in% rownames(installed_pkgs)) {
  tryCatch({
    library(funfun)
    cat("✅ funfun loaded successfully\n")

    # Check available datasets and functions
    cat("   Available objects: ", paste(ls("package:funfun"), collapse=", "), "\n")

  }, error = function(e) {
    cat("❌ Error loading funfun: ", e$message, "\n")
  })
} else {
  cat("❌ funfun not installed, skipping test\n")
}

cat("\n=== Package Testing Complete ===\n")