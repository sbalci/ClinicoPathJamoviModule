# Internationalization (i18n) Translation Plan: timeinterval → Turkish (TR)

## Analysis: Time Interval Calculator

**Description**: Calculates exact elapsed follow-up duration between clinical milestone dates (diagnosis, surgery, recurrence, last visit).

### 0) Argument Normalization

- **Sanitized Function**: `timeinterval`
- **YAML Schema Files**:
  - `jamovi/timeinterval.a.yaml` (Options schema)
  - `jamovi/timeinterval.u.yaml` (UI layout schema)
  - `jamovi/timeinterval.r.yaml` (Results presentation schema)
- **R Backend File**:
  - `R/timeinterval.b.R` (Core implementation)

---

## 1) Translation Metrics

- **Total Function-Specific Strings**: 59
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
| `{count} date value(s) were written with a two-digit year. These are read as 2...` | `{count} tarih değeri iki basamaklı bir yıl ile yazılmıştır. Bunlar 00-68 için...` |
| `{count} date values are in the future. Review date columns for data entry err...` | `{count} tarih değeri gelecekte. Veri giriş hataları veya yanlış tarih biçimle...` |
| `{count} observations ({pct}%) have missing time intervals. Investigate missin...` | `{count} gözlemde (%{pct}) kayıp zaman aralığı var. Çalışma sonuçlarını etkile...` |
| `{pct}% of intervals are zero-length (start and end date on the same day). The...` | `Aralıkların %{pct}'si sıfır uzunluktadır (başlangıç ve bitiş tarihi aynı gün)...` |
| `A third cause makes every row negative by less than a day: a start column car...` | `Üçüncü bir neden her satırı bir günden daha az bir süreyle negatif yapar: gec...` |
| `Advanced time interval analysis with quality assessment` | `Kalite değerlendirmesi ile ileri düzey zaman aralığı analizi` |
| `Advanced time interval calculator designed for survival analysis, epidemiolog...` | `Sağkalım analizi, epidemiyolojik çalışmalar ve kişi-zamanı analizi için tasar...` |
| `All observations were removed by the data quality filters ({n} rows). Switch ...` | `Tüm gözlemler veri kalitesi filtreleri tarafından kaldırıldı ({n} satır). Ham...` |
| `Analysis completed using {n} observations that reached the {landmark} landmar...` | `Analiz, {landmark} yer işaretine ulaşan {n} gözlem kullanılarak tamamlandı. B...` |
| `Analysis completed using {n} observations with mean follow-up {mean} {unit} (...` | `Analiz, ortalama takip süresi {mean} {unit} olan {n} gözlem kullanılarak tama...` |
| `At this share the fault is systematic rather than sporadic: the rows that wer...` | `Bu oranda hata düzensiz olmaktan ziyade sistematiktir: tutulan satırlar aynı ...` |
| `Auto-detection is ambiguous: {formats} all parse these dates equally well ({p...` | `Otomatik algılama belirsiz: {formats} biçimlerinin tümü bu tarihleri eşit der...` |
| `Calculated Time ({unit}, from {landmark} {unit} landmark)` | `Hesaplanan Süre ({unit}, {landmark} {unit} yer işaretinden itibaren)` |
| `Calculated Time ({unit})` | `Hesaplanan Süre ({unit})` |
| `Column '{column}' contains only missing values; cannot calculate time intervals.` | `'{column}' sütunu yalnızca eksik değerler içeriyor; zaman aralıkları hesaplan...` |
| `Column '{column}' holds five-digit numbers such as {value}, which cannot be r...` | `'{column}' sütunu, tarih olarak belirsiz olmaksızın okunamayan {value} gibi b...` |
| `Comprehensive Time Interval Calculator` | `Kapsamlı Zaman Aralığı Hesaplayıcısı` |
| `Correct the dates at source, or tick 'Remove negative intervals' under Data Q...` | `Tarihleri kaynakta düzeltin veya bu satırları kişi-zamanı dahil her istatisti...` |
| `Could not detect a common date format for columns '{start}' and '{end}'. Plea...` | `'{start}' ve '{end}' sütunları için ortak bir tarih biçimi algılanamadı. Lütf...` |
| `Counted from the spreadsheet epoch, {value} would be {date}; SAS and Stata co...` | `Elektronik tablo başlangıcından sayıldığında {value}, {date} olur; SAS ve Sta...` |
| `Critically small sample (n={n}). Statistical summaries are unreliable with fe...` | `Kritik derecede küçük örneklem (n={n}). 10'dan az gözlemle istatistiksel özet...` |
| `Data frame is empty; ensure your dataset has at least one row.` | `Veri çerçevesi boş; veri setinizin en az bir satıra sahip olduğundan emin olun.` |
| `Date parsing failed for column '{column}' using format '{format}'. Example va...` | `'{column}' sütunu için '{format}' biçimi kullanılarak tarih ayrıştırma başarı...` |
| `End date column contains only missing values; cannot calculate time intervals.` | `Bitiş tarihi sütunu yalnızca kayıp değerler içeriyor; zaman aralıkları hesapl...` |
| `End dates are not valid date objects` | `Bitiş tarihleri geçerli tarih nesneleri değil` |
| `Error parsing dates with format {format}: {message}` | `{format} biçimindeki tarihler ayrıştırılırken hata oluştu: {message}` |
| `Examples:` | `Örnekler:` |
| `Extreme-value filtering cannot act on these data: with {n} intervals (fewer t...` | `Aşırı değer filtrelemesi bu veriler üzerinde işlem yapamaz: {n} aralık ile (1...` |
| `Extreme-value filtering was skipped: the 99th percentile of the intervals is ...` | `Aşırı değer filtrelemesi atlandı: aralıkların 99. yüzdeliği {q99} olduğundan,...` |
| `Extreme-value removal dropped {count} of the longest interval(s) (above {thre...` | `Aşırı değerlerin kaldırılması, analizden en uzun aralık(lar)ın {count} tanesi...` |
| `Fix the source data: format the column as a date before exporting (in Excel: ...` | `Kaynak verileri düzeltin: dışa aktarmadan önce sütunu tarih olarak biçimlendi...` |
| `Guessed as a packed date instead, the same digits give a different year, so t...` | `Bunun yerine sıkıştırılmış bir tarih olarak tahmin edildiğinde, aynı basamakl...` |
| `Info` | `Bilgi` |
| `Landmark analysis excluded every participant: none of the {n} observations re...` | `Yer işareti analizi tüm katılımcıları hariç tuttu: {n} gözlemin hiçbiri {amou...` |
| `Landmark time must be a non-negative number` | `Yer işareti zamanı negatif olmayan bir sayı olmalıdır` |

