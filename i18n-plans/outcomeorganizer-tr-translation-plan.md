# Internationalization (i18n) Translation Plan: outcomeorganizer → Turkish (TR)

## Analysis: Outcome Organizer

**Description**: Harmonizes vital status, recurrence, cause of death, and progression into standardized survival endpoints (OS, PFS, DFS, DSS).

### 0) Argument Normalization

- **Sanitized Function**: `outcomeorganizer`
- **YAML Schema Files**:
  - `jamovi/outcomeorganizer.a.yaml` (Options schema)
  - `jamovi/outcomeorganizer.u.yaml` (UI layout schema)
  - `jamovi/outcomeorganizer.r.yaml` (Results presentation schema)
- **R Backend File**:
  - `R/outcomeorganizer.b.R` (Core implementation)

---

## 1) Translation Metrics

- **Total Function-Specific Strings**: 12
- **Translation Coverage**: 100% Complete
- **Validation Status**: 0 placeholder mismatches, 0 HTML tag mismatches, 0 context traps

---

## 2) Verification & Testing Commands

```bash
# Validate Turkish catalog syntax and compilation
msgfmt -v -c -o /dev/null jamovi/i18n/tr.po

# Run package unit tests
Rscript -e "devtools::test()"
```

---

## 3) Translation Sample Dictionary (First 35 Strings)

| English Source (`msgid`) | Turkish Translation (`msgstr`) |
| :--- | :--- |
| `'{analysis}' is not available with Multiple Event Levels enabled. Recurrence-...` | `Çoklu Olay Düzeyleri etkinken '{analysis}' kullanılamaz. Nükse dayalı sonlanı...` |
| `A recurrence/progression variable is selected for {analysis} analysis, but no...` | `{analysis} analizi için bir nüks/ilerleme değişkeni seçildi, ancak nüks olay ...` |
| `A recurrence/progression variable is selected for TTP analysis, but no progre...` | `TTP analizi için bir nüks/ilerleme değişkeni seçildi, ancak ilerleme olay düz...` |
| `Advanced tool for preparing outcome variables for various types of survival a...` | `Genel sağkalım, nedene özgü, yarışan riskler, ilerlemesiz sağkalım ve çok dur...` |
| `Comprehensive Outcome Preparation for Survival Analysis` | `Sağkalım Analizi İçin Kapsamlı Sonuç Değişkeni Hazırlığı` |
| `Could not find outcome variable` | `Sonuç değişkeni bulunamadı` |
| `Each outcome level may be assigned to only one state. Assigned to more than o...` | `Her sonuç düzeyi yalnızca tek bir duruma atanabilir. Birden fazlasına atananl...` |
| `Error cleaning variable names. Please check column names.` | `Değişken adları temizlenirken hata oluştu. Lütfen sütun adlarını kontrol edin.` |
| `Outcome level(s) not assigned to any state: {levels}. Assign every level to o...` | `Hiçbir duruma atanmamış sonuç düzey(ler)i: {levels}. Her düzeyi dört durumdan...` |
| `Outcome Organizer for Survival Analysis` | `Sağkalım Analizi İçin Sonuç Düzenleyici` |
| `Outcome recoding failed: all values are NA. This usually means the selected o...` | `Sonuç yeniden kodlama başarısız oldu: tüm değerler NA. Bu genellikle seçilen ...` |
| `Priority Event Type must be 1 or greater (it is {value}). Code 0 is the censo...` | `Öncelikli Olay Türü 1 veya daha büyük olmalıdır ({value} olarak girildi). 0 k...` |

