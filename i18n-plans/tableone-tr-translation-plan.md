# Turkish (TR) Internationalization Plan for tableone Function

## File Analysis

**SANITIZED_FN**: `tableone`

✅ **Files Found:**
- `jamovi/tableone.a.yaml` (options)
- `jamovi/tableone.u.yaml` (UI) 
- `jamovi/tableone.r.yaml` (results)
- `R/tableone.b.R` (backend)

✅ **NAMESPACE**: Already includes `importFrom(jmvcore,.)`

---

## 2) Translatable Strings Found in R/tableone.b.R

### 2.1 Error Messages (Lines 29, 78, 91, 112, 128, 135, 170-172, 202)
### 2.2 HTML Content & Labels (Lines 34-45, 158, 161, 164, 176)

---

## 3) Code Patches Required

### 3.1 Error Message Wrapping

```diff
- stop("Error: The input data contains no (complete) rows. Please provide a valid dataset.")
+ stop(.("Error: The input data contains no (complete) rows. Please provide a valid dataset."))

- stop("Error in tableone package: ", e$message)
+ stop(.("Error in tableone package: {message}"), message = e$message)

- stop("Error in gtsummary package: ", e$message)  
+ stop(.("Error in gtsummary package: {message}"), message = e$message)

- stop("Error in arsenal package: ", e$message)
+ stop(.("Error in arsenal package: {message}"), message = e$message)

- stop("Variable '", var, "' not found in data")
+ stop(.("Variable '{var}' not found in data"), var = var)

- stop("Variable '", var, "' has no non-missing values")
+ stop(.("Variable '{var}' has no non-missing values"), var = var)

- stop("Error processing variable '", var, "' with janitor: ", e$message, 
       " (Variable type: ", class(data[[var]])[1], 
       ", Non-missing values: ", sum(!is.na(data[[var]])), ")")
+ stop(.("Error processing variable '{var}' with janitor: {message} (Variable type: {type}, Non-missing values: {count})"), 
       var = var, message = e$message, type = class(data[[var]])[1], count = sum(!is.na(data[[var]])))

- stop("Error: Invalid table style selected. Please choose a valid style.")
+ stop(.("Error: Invalid table style selected. Please choose a valid style."))
```

### 3.2 HTML Content Wrapping

```diff
- todo_message <- "
-   <br><strong>Welcome to the ClinicoPath Table One Generator</strong>
-   <br><br>
-   <strong>Instructions:</strong>
-   <ul>
-       <li>Select the <em>Variables</em> to include in the Table One. (Numeric, Ordinal, or Categorical)</li>
-       <li>Choose a <em>Table Style</em> for the output format.</li>
-       <li>If needed, check the option to <em>Exclude Missing Values</em> (NA). (Exclusion may remove entire cases.)</li>
-   </ul>
-   <br>
-   Please ensure you cite the packages and jamovi as referenced below.
- "

+ todo_message <- paste0(
+   "<br><strong>", .("Welcome to the ClinicoPath Table One Generator"), "</strong>",
+   "<br><br>",
+   "<strong>", .("Instructions:"), "</strong>",
+   "<ul>",
+       "<li>", .("Select the Variables to include in the Table One. (Numeric, Ordinal, or Categorical)"), "</li>",
+       "<li>", .("Choose a Table Style for the output format."), "</li>", 
+       "<li>", .("If needed, check the option to Exclude Missing Values (NA). (Exclusion may remove entire cases.)"), "</li>",
+   "</ul>",
+   "<br>",
+   .("Please ensure you cite the packages and jamovi as referenced below."),
+ )
```

### 3.3 Table Column Headers

```diff
- names(table)[2] <- "N"
+ names(table)[2] <- .("N")

- names(table)[3] <- "Percent" 
+ names(table)[3] <- .("Percent")

- names(table)[4] <- "Valid Percent"
+ names(table)[4] <- .("Valid Percent")
```

### 3.4 Dynamic Headers

```diff
- header <- paste0("<h4 style='margin-top:20px;'>Frequency Table for '", var, "'</h4>")
+ header <- paste0("<h4 style='margin-top:20px;'>", .("Frequency Table for '{var}'"), "</h4>")
+ # Note: Use jmvcore::format() later: jmvcore::format(.("Frequency Table for '{var}'"), list(var = var))
```

---

## 4) Translation Commands

### 4.1 Create/Update Catalogs

```r
# Create/Update English template 
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en")

# Create/Update Turkish catalog
jmvtools::i18nCreate("tr") 
jmvtools::i18nUpdate("tr")
```

### 4.2 Prepare POT File

```bash
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
# Edit header to include: Language: c\n
```

---

## 5) Turkish Translation Table

| Status | msgid | Suggested Turkish Translation |
|--------|-------|------------------------------|
| missing | "Welcome to the ClinicoPath Table One Generator" | "ClinicoPath Tablo Bir Oluşturucusuna Hoş Geldiniz" |
| missing | "Instructions:" | "Talimatlar:" |
| missing | "Select the Variables to include in the Table One. (Numeric, Ordinal, or Categorical)" | "Tablo Bir'e dahil edilecek Değişkenleri seçin. (Sayısal, Sıralı veya Kategorik)" |
| missing | "Choose a Table Style for the output format." | "Çıktı formatı için bir Tablo Stili seçin." |
| missing | "If needed, check the option to Exclude Missing Values (NA). (Exclusion may remove entire cases.)" | "Gerekirse, Eksik Değerleri Hariç Tut (NA) seçeneğini işaretleyin. (Hariç tutma tüm vakaları kaldırabilir.)" |
| missing | "Please ensure you cite the packages and jamovi as referenced below." | "Lütfen aşağıda referans verilen paketleri ve jamovi'yi kaynak olarak gösterdiğinizden emin olun." |
| missing | "N" | "N" |
| missing | "Percent" | "Yüzde" |
| missing | "Valid Percent" | "Geçerli Yüzde" |
| missing | "Frequency Table for '{var}'" | "'{var}' için Frekans Tablosu" |
| missing | "Error: The input data contains no (complete) rows. Please provide a valid dataset." | "Hata: Giriş verisi hiç (tam) satır içermemektedir. Lütfen geçerli bir veri kümesi sağlayın." |
| missing | "Error in tableone package: {message}" | "tableone paketinde hata: {message}" |
| missing | "Error in gtsummary package: {message}" | "gtsummary paketinde hata: {message}" |
| missing | "Error in arsenal package: {message}" | "arsenal paketinde hata: {message}" |
| missing | "Variable '{var}' not found in data" | "'{var}' değişkeni veride bulunamadı" |
| missing | "Variable '{var}' has no non-missing values" | "'{var}' değişkeninin eksik olmayan değeri yok" |
| missing | "Error processing variable '{var}' with janitor: {message} (Variable type: {type}, Non-missing values: {count})" | "janitor ile '{var}' değişkeni işlenirken hata: {message} (Değişken tipi: {type}, Eksik olmayan değer sayısı: {count})" |
| missing | "Error: Invalid table style selected. Please choose a valid style." | "Hata: Geçersiz tablo stili seçildi. Lütfen geçerli bir stil seçin." |

---

## 6) Clinical Turkish Glossary

```text
Table One → Tablo Bir / Birinci Tablo
Descriptive Statistics → Tanımlayıcı İstatistikler  
Frequency Table → Frekans Tablosu
Missing Values → Eksik Değerler
Variables → Değişkenler
Categorical → Kategorik
Ordinal → Sıralı  
Numeric → Sayısal
Percent → Yüzde
Valid Percent → Geçerli Yüzde
Dataset → Veri Kümesi
Error → Hata
Package → Paket
```

---

## 7) QA Checklist

- ✅ All expected jamovi files exist
- ✅ NAMESPACE includes i18n helper import
- 🔲 Apply .() wrapping patches to R/tableone.b.R
- 🔲 Run i18nCreate/Update commands  
- 🔲 Validate Turkish translations for clinical accuracy
- 🔲 Test module compilation after i18n changes

---

## 8) Implementation Steps

### Step 1: Apply Code Patches

Apply all the diff patches above to `R/tableone.b.R` to wrap user-visible strings.

### Step 2: Generate Catalogs 

```r
jmvtools::i18nCreate("en")
jmvtools::i18nUpdate("en") 
jmvtools::i18nCreate("tr")
jmvtools::i18nUpdate("tr")
```

### Step 3: Translate Turkish Strings

Edit `jamovi/i18n/tr.po` to add the Turkish translations from the table above.

### Step 4: Test & Validate

```r
# Test module compilation
jmvtools::prepare()

# Test Turkish locale (if available)
Sys.setlocale("LC_ALL", "tr_TR.UTF-8")
```

---

## 9) Weblate Integration (Future)

1. Create dedicated repo: `ClinicoPath-i18n`
2. Add catalog.pot, README.md, license  
3. Add Weblate bot as collaborator
4. Configure webhook: `https://hosted.weblate.org/hooks/github/`
5. Contact jamovi dev team to add project to Weblate

---

## 10) Ready-to-Run Commands

```bash
# Quick check for unwrapped strings (after patches applied)
grep -nE '"[^"]*[A-Za-z][^"]*"' R/tableone.b.R | grep -v '\\.\('

# Generate catalogs
Rscript -e "jmvtools::i18nCreate('en'); jmvtools::i18nUpdate('en'); jmvtools::i18nCreate('tr'); jmvtools::i18nUpdate('tr')"

# Prepare POT for Weblate  
cp jamovi/i18n/en.po jamovi/i18n/catalog.pot
```

This plan provides complete Turkish localization support for the tableone function, covering all user-visible strings with clinical terminology appropriate for pathologists and clinicians.