# Test script to check fungal trait packages

# =============================================================================
# PERFORMANCE SCORING FRAMEWORK
# =============================================================================

# Global counters for test results
total_tests <- 0
passed_tests <- 0
failed_tests <- 0

# Helper function for scoring
test_result <- function(passed, test_name, details = "") {
  total_tests <<- total_tests + 1
  if (passed) {
    passed_tests <<- passed_tests + 1
    cat("✅ PASS:", test_name, "\n")
  } else {
    failed_tests <<- failed_tests + 1
    cat("❌ FAIL:", test_name, "\n")
  }
  if (details != "") {
    cat("   ", details, "\n")
  }
}

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

# Score package installation check
installed_count <- sum(fungal_pkgs %in% rownames(installed_pkgs))
test_result(installed_count >= 1, "Package installation", 
           paste0(installed_count, "/", length(fungal_pkgs), " fungal packages installed"))

cat("\n2. Checking available packages on CRAN...\n")
available_pkgs <- available.packages()

available_count <- 0
for (pkg in fungal_pkgs) {
  if (pkg %in% rownames(available_pkgs)) {
    cat("✅ ", pkg, " is available on CRAN\n")
    cat("   Description: ", strwrap(available_pkgs[pkg, "Title"], width=60)[1], "...\n")
    available_count <- available_count + 1
  } else {
    cat("❌ ", pkg, " is not available on CRAN\n")
  }
}

# Score CRAN availability
test_result(available_count >= 1, "CRAN availability", 
           paste0(available_count, "/", length(fungal_pkgs), " packages available on CRAN"))

cat("\n3. Testing fungaltraits if available...\n")
fungaltraits_works <- FALSE
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
        fungaltraits_works <- TRUE
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

# Score fungaltraits functionality
test_result(fungaltraits_works, "fungaltraits functionality", 
           ifelse(fungaltraits_works, "fungaltraits loaded with mycorrhizal data available", 
                  "fungaltraits not working or missing mycorrhizal data"))

cat("\n4. Testing funfun if available...\n")
funfun_works <- FALSE
if ("funfun" %in% rownames(installed_pkgs)) {
  tryCatch({
    library(funfun)
    cat("✅ funfun loaded successfully\n")

    # Check available datasets and functions
    cat("   Available objects: ", paste(ls("package:funfun"), collapse=", "), "\n")
    funfun_works <- TRUE

  }, error = function(e) {
    cat("❌ Error loading funfun: ", e$message, "\n")
  })
} else {
  cat("❌ funfun not installed, skipping test\n")
}

# Score funfun functionality
test_result(funfun_works, "funfun functionality", 
           ifelse(funfun_works, "funfun package loaded successfully", 
                  "funfun not working or not installed"))

cat("\n=== Package Testing Complete ===\n")

# =============================================================================
# PERFORMANCE SUMMARY
# =============================================================================

cat("\n=== PERFORMANCE SUMMARY ===\n")
cat("Total Tests:", total_tests, "\n")
cat("Passed:", passed_tests, "(", round(passed_tests/total_tests * 100, 1), "%)\n")
cat("Failed:", failed_tests, "(", round(failed_tests/total_tests * 100, 1), "%)\n")

if (failed_tests == 0) {
  cat("🎉 All tests passed! Fungal packages are properly configured.\n")
} else {
  cat("⚠️ Some tests failed. Check the output above for details.\n")
}