# Internationalization (i18n) Translation Plan: datetimeconverter → Turkish (TR)

## Analysis: Date & Time Converter

**Description**: Converts, validates, and normalizes clinical dates across various international and clinical formats.

### 0) Argument Normalization

- **Sanitized Function**: `datetimeconverter`
- **YAML Schema Files**:
  - `jamovi/datetimeconverter.a.yaml` (Options schema)
  - `jamovi/datetimeconverter.u.yaml` (UI layout schema)
  - `jamovi/datetimeconverter.r.yaml` (Results presentation schema)
- **R Backend File**:
  - `R/datetimeconverter.b.R` (Core implementation)

---

## 1) Translation Metrics

- **Total Function-Specific Strings**: 51
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
| `; {missing} of the {total} rows were missing` | `; {total} satırın {missing} tanesi kayıptı` |
| `{count} date(s) are in the future. Verify this is intentional (for example pl...` | `{count} tarih gelecekte yer alıyor. Bunun kasıtlı olduğunu doğrulayın (örneği...` |
| `{count} date(s) fall before 1900, which usually means the format was read wro...` | `{count} tarih 1900 yılından öncesine denk geliyor; bu genellikle biçimin yanl...` |
| `{count} of {total} dates ({pct}%) are in the future, and this column has two-...` | `{total} tarihten {count} tanesi (%{pct}) gelecekte yer alıyor ve bu sütunda i...` |
| `{count} of {total} parsed date(s) fall outside the plausible range (<1900 or ...` | `Ayrıştırılan {total} tarihten {count} tanesi makul aralığın dışına çıkıyor (<...` |
| `{pct}% of non-missing values produced a date, but see the warnings above: a c...` | `Eksik olmayan değerlerin %{pct}'si bir tarih üretti, ancak yukarıdaki uyarıla...` |
| `A text date format is selected but this column is numeric. • Choose Excel Ser...` | `Metin tarih biçimi seçildi ancak bu sütun sayısaldır. • Bunlar gerçekten seri...` |
| `All values in {name} are missing (NA). • Please select a column with valid da...` | `{name} içindeki tüm değerler kayıptır (NA). • Lütfen devam etmeden önce geçer...` |
| `All values in the selected variable are missing (NA). • Select a different va...` | `Seçilen değişkendeki tüm değerler kayıptır (NA). • Farklı bir değişken seçin ...` |
| `All Values Missing` | `Tüm Değerler Kayıp` |
| `Ambiguous Format Detected` | `Belirsiz Biçim Algılandı` |
| `Conversion Completed` | `Dönüştürme Tamamlandı` |
| `Convert datetime and extract components` | `Tarih-saati dönüştürün ve bileşenleri ayıklayın` |
| `Convert datetime variables to standardized format and extract datetime compon...` | `Tarih-saat değişkenlerini standart biçime dönüştürün ve tarih-saat bileşenler...` |
| `Could not reliably detect the datetime format. • The closest match was {fmt},...` | `Tarih-saat biçimi güvenilir şekilde algılanamadı. • En yakın eşleşme, örnekle...` |
| `Data Preparation` | `Veri Hazırlama` |
| `Dates Read Into The Wrong Century` | `Yanlış Yüzyıla Okunan Tarihler` |
| `DateTime conversion completed. • Processed {rows} rows from variable {name}. ...` | `Tarih-Saat dönüştürme tamamlandı. • {name} değişkeninden {rows} satır işlendi...` |
| `DateTime Converter` | `Tarih-Saat Dönüştürücü` |
| `DateTime Format is set to a numeric format ({fmt}) but this column does not h...` | `Tarih-Saat Biçimi sayısal bir biçime ({fmt}) ayarlandı ancak bu sütun sayısal...` |
| `Empty Dataset` | `Boş Veri Seti` |
| `Error parsing datetimes with format {fmt}. • Parser error: {msg} • Try select...` | `{fmt} biçimindeki tarih-saatler ayrıştırılırken hata oluştu. • Ayrıştırıcı ha...` |
| `Excel serial numbers do not record which epoch they came from, so the 1900 sy...` | `Excel seri numaraları hangi başlangıç noktasından (epoch) geldiklerini kaydet...` |
| `Excel Serial Origin Assumed (1900 System)` | `Excel Seri Başlangıcı Varsayıldı (1900 Sistemi)` |
| `Format Detection Failed` | `Biçim Algılama Başarısız Oldu` |
| `Implausible Dates Detected` | `Mantıksız Tarihler Tespit Edildi` |
| `Invalid Timezone` | `Geçersiz Saat Dilimi` |
| `Low datetime parsing success rate: {pct}% • Only {parsed} of {total} non-miss...` | `Düşük tarih-saat ayrıştırma başarı oranı: %{pct} • Eksik olmayan {total} değe...` |
| `Low Parsing Success Rate` | `Düşük Ayrıştırma Başarı Oranı` |
| `Manual format selection ({fmt}) was ignored because the column is already sto...` | `Sütun zaten tarih-saat değerleri olarak saklandığından manuel biçim seçimi ({...` |
| `Moderate datetime parsing success rate: {pct}% • {parsed} of {total} non-miss...` | `Orta düzeyde tarih-saat ayrıştırma başarı oranı: %{pct} • Eksik olmayan {tota...` |
| `Moderate Parsing Success Rate` | `Orta Düzeyde Ayrıştırma Başarı Oranı` |
| `No Valid Datetime Values Found` | `Geçerli Tarih-Saat Değeri Bulunamadı` |
| `Numeric Column May Not Be Dates` | `Sayısal Sütun Tarih Olmayabilir` |
| `Numeric Column May Not Be Unix Timestamps` | `Sayısal Sütun Unix Zaman Damgası Olmayabilir` |

