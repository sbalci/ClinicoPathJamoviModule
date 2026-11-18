# Turkish (`tr.po`) Translation Plan — `jjdotplotstats`

- **Analysis**: `jjdotplotstats` (*Horizontal Box-Violin Comparison* / *Yatay Kutu-Keman Karşılaştırması*)
- **Total catalog entries referencing `jjdotplotstats`**: 229
- **Existing shared module translations preserved**: 227
- **Newly populated Turkish translations**: 2
- **Untranslated / fuzzy / `msgctxt`-split entries**: `0`

## Terminology Glossary Applied

| English | Turkish (`tr.po`) |
| :--- | :--- |
| Horizontal Box-Violin Comparison | Yatay Kutu-Keman Karşılaştırması |
| Confidence Interval / Credible Interval | Güven Aralığı / İnanılırlık Aralığı |
| Effect Size Measure | Etki Büyüklüğü Ölçütü |
| Pairwise Comparisons | İkili Karşılaştırmalar |
| Multiple Comparison Adjustment | Çoklu Karşılaştırma Düzeltmesi |
| Split By (Optional) | Alt Gruba Göre Böl (İsteğe Bağlı) |
| Central Tendency Measure | Merkezi Eğilim Ölçütü |
| Okabe-Ito (color-blind safe) | Okabe-Ito (renk körlüğüne uygun) |
| Viridis (perceptually uniform) | Viridis (algısal olarak tekdüze) |

## Catalog Entries (`jjdotplotstats`)

| Status | `msgid` | `msgstr` (TR) |
| :--- | :--- | :--- |
| existing | '{name}' takes the same value ({value}) in every row, so there is no variation to compare between groups. Check that the correct variable is selected. | '{name}' değişkeni her satırda aynı değeri ({value}) almaktadır; bu nedenle gruplar arasında karşılaştırılacak bir değişim yoktur. Doğru değişkenin seçildiğini kontrol edin. |
| existing | 'Central Tendency Precision' no longer has any effect - the statistics package dropped that setting. Use 'Statistical Precision (Decimal Places)' instead; it controls the centrality labels too. | 'Merkezi Eğilim Hassasiyeti' seçeneğinin artık bir etkisi yoktur; istatistik paketi bu ayarı kaldırmıştır. Bunun yerine merkezi eğilim etiketlerini de kontrol eden 'İstatistiksel Hassasiyet (Ondalık Basamak)' ayarını kullanın. |
| existing | 'Effect Size Measure' applies to the parametric test only. The {test} test reports its own effect size instead, so your selection was not used. | 'Etki Büyüklüğü Ölçütü' yalnızca parametrik test için geçerlidir. {test} testi kendi etki büyüklüğünü raporladığından seçiminiz kullanılmamıştır. |
| existing | {names} had no usable measurements and were excluded from the comparison entirely. | {names} için kullanılabilir ölçüm bulunmadığından bu grup(lar) karşılaştırmadan tamamen çıkarılmıştır. |
| existing | `${dep} - ${group} by ${grvar}` | `${dep} - ${group} (${grvar} ile)` |
| existing | `${dep} - ${group}` | `${dep} - ${group}` |
| existing | a Bayes factor is only defined here for the parametric test | Bayes faktörü burada yalnızca parametrik test için tanımlıdır |
| existing | a Bayesian one-way analysis of variance | Bayesyen tek yönlü varyans analizi |
| existing | a Bayesian t-test | Bayesyen t-testi |
| existing | A centrality type is selected but centrality plotting is switched off, so the type setting has no effect. | Bir merkezi eğilim türü seçilmiş ancak merkezi eğilim çizimi kapalı olduğundan tür ayarının bir etkisi yoktur. |
| existing | A comparison of {outcome} across {groups} groups of {factor} was performed using {test}, with {effect} reported as the effect size and a {level} per cent {interval} interval. | {factor} değişkeninin {groups} grubu arasında {outcome} karşılaştırması {test} kullanılarak yapılmış; etki büyüklüğü olarak {effect} ve yüzde {level} {interval} aralığı raporlanmıştır. |
| existing | a heteroscedastic one-way comparison of trimmed means | budanmış ortalamaların değişen varyanslı tek yönlü karşılaştırması |
| existing | Across N = {total} complete observations ({groupsummary}), {test} could not be evaluated ({reason}), so no omnibus test statistic, interval, or post-hoc pairwise brackets were produced. | Eksiksiz N = {total} gözlem genelinde ({groupsummary}), {test} değerlendirilemedi ({reason}); bu nedenle genel test istatistiği, aralık veya post-hoc ikili karşılaştırma parantezleri üretilmedi. |
| existing | Across N = {total} complete observations ({groupsummary}), {test} gave {statname} = {statval}, p = {pval} ({verdict}), with {effname} = {effval} ({level} per cent confidence interval {cilow} to {cihigh}). | Eksiksiz N = {total} gözlem genelinde ({groupsummary}), {test} sonucunda {statname} = {statval}, p = {pval} ({verdict}) ve {effname} = {effval} (yüzde {level} güven aralığı {cilow} ile {cihigh}) elde edildi. |
| existing | Across N = {total} complete observations ({groupsummary}), {test} yielded log_e(BF_01) = {logbf} ({evidence}), with {effname} = {effval} ({level} per cent credible interval {cilow} to {cihigh}). | Eksiksiz N = {total} gözlem genelinde ({groupsummary}), {test} sonucunda log_e(BF_01) = {logbf} ({evidence}) ve {effname} = {effval} (yüzde {level} inanılırlık aralığı {cilow} ile {cihigh}) elde edildi. |
| existing | All pairs | Tüm çiftler |
| existing | Analysis summary: {groups} groups, {total} total observations. | Analiz özeti: {groups} grup, toplam {total} gözlem. |
| existing | Assumptions and interpretation | Varsayımlar ve yorumlama |
| existing | Assumptions and Interpretation | Varsayımlar ve Yorumlama |
| existing | At least two groups with data are required for a comparison. Groups found with complete data: {found}. Please check for missing values or select different variables. | Karşılaştırma için verisi olan en az iki grup gereklidir. Eksiksiz verisi bulunan grup sayısı: {found}. Lütfen kayıp değerleri kontrol edin veya farklı değişkenler seçin. |
| existing | Bayes factor caption | Bayes faktörü alt yazısı |
| added | BayesFactor package unavailable | BayesFactor paketi kullanılamıyor |
| existing | Bayesian (JZS Bayes factors and posterior estimation) | Bayesyen (JZS Bayes faktörleri ve sonsal kestirim) |
| existing | Bayesian Estimate (MAP estimator) | Bayesyen Kestirim (MAP kestiricisi) |
| existing | Bayesian pairwise comparisons report natural-log JZS Bayes factors rather than frequentist p values, so all group pairs are shown on the plot. | Bayesyen ikili karşılaştırmalar sıklıkçı p-değerleri yerine doğal logaritmalı JZS Bayes faktörlerini raporladığından grafikte tüm grup çiftleri gösterilir. |
| existing | Bayesian posterior quantities and credible intervals use Markov chain Monte Carlo sampling with fixed random seed {seed} for exact reproducibility. | Bayesyen sonsal büyüklükler ve inanılırlık aralıkları, tam tekrarlanabilirlik için sabit rastgelelik tohumu ({seed}) ile Markov zinciri Monte Carlo örneklemesi kullanır. |
| existing | Bayesian t-test returned no expression | Bayesyen t-testi ifade döndürmedi |
| existing | Before you report this | Raporlamadan önce kontrol edin |
| existing | Benjamini-Hochberg | Benjamini-Hochberg |
| existing | Benjamini-Hochberg (false discovery rate) | Benjamini-Hochberg (yanlış keşif oranı) |
| existing | Bias-corrected - Hedges' g with 2 groups, omega-squared with 3+ | Yanlılık düzeltmeli - 2 grupta Hedges g, 3+ grupta omega-kare |
| existing | Bonferroni | Bonferroni |
| existing | Bonferroni (most conservative) | Bonferroni (en tutucu) |
| existing | Categorical vs Continuous | Kategorik ve Sürekli |
| existing | Central Tendency Display | Merkezi Eğilim Gösterimi |
| existing | Central tendency lines | Merkezi eğilim çizgileri |
| existing | Central Tendency Measure | Merkezi Eğilim Ölçütü |
| existing | Central Tendency Precision (no longer used) | Merkezi Eğilim Hassasiyeti (artık kullanılmıyor) |
| existing | Centrality Display | Merkezi Eğilim Gösterimi |
| existing | Centrality Options | Merkezi Eğilim Seçenekleri |
| existing | Centrality plotting is enabled but the centrality parameter is set to none, so no centrality lines will be displayed. | Merkezi eğilim çizimi etkinleştirilmiş ancak merkezi eğilim parametresi yok olarak ayarlandığından merkezi eğilim çizgisi gösterilmeyecektir. |
| existing | Cohen's d | Cohen's d |
| existing | Color palette | Renk paleti |
| existing | Compares a continuous variable across independent groups and draws the comparison horizontally - values along the x axis and group labels down the y axis - with violins, boxplots, individual observations, and an optional vertical reference threshold line. Wraps ggstatsplot between-group comparisons using Welch's t-test or Welch's ANOVA (parametric), Mann-Whitney U or Kruskal-Wallis (nonparametric), Yuen or Wilcox 20 percent trimmed-mean tests (robust), and Jeffreys-Zellner-Siow Bayes factors (Bayesian). For a Cleveland dot chart with one summary point per group tested against a reference value, use Dot Chart (Summary vs Reference Value) instead. | Sürekli bir değişkeni bağımsız gruplar arasında karşılaştırır ve karşılaştırmayı yatay olarak çizer (değerler x ekseninde, grup etiketleri y ekseninde); keman grafiği, kutu grafiği, bireysel gözlemler ve isteğe bağlı dikey referans eşik çizgisi içerir. Welch t-testi veya Welch ANOVA (parametrik), Mann-Whitney U veya Kruskal-Wallis (parametrik olmayan), Yuen veya Wilcox yüzde 20 budanmış ortalama testleri (dayanıklı) ve Jeffreys-Zellner-Siow Bayes faktörlerini (Bayesyen) kullanarak ggstatsplot gruplar arası karşılaştırmalarını sunar. Grup başına tek özet noktanın referans değere karşı test edildiği Cleveland nokta grafiği için bunun yerine Nokta Grafiği (Özet ve Referans Değer) analizini kullanın. |
| existing | Comparing {groups} groups with N = {n} observations using a {test} test. | {groups} grup, N = {n} gözlem ile {test} testi kullanılarak karşılaştırılıyor. |
| existing | confidence | güven |
| existing | Confidence Level | Güven Düzeyi |
| existing | credible | inanılırlık |
| existing | Dark2 (classic ggstatsplot) | Dark2 (klasik ggstatsplot) |
| existing | Data contains no complete rows after filtering. Please check for missing values. | Filtreleme sonrasında veride eksiksiz satır kalmadı. Lütfen kayıp değerleri kontrol edin. |
| existing | Data contains no complete rows. Please check for missing values in your selected variables. | Veride eksiksiz satır bulunmamaktadır. Lütfen seçtiğiniz değişkenlerdeki kayıp değerleri kontrol edin. |
| existing | Data processing failed: {reason}. Please check your variable selections and try again. | Veri işleme başarısız oldu: {reason}. Lütfen değişken seçimlerinizi kontrol edip tekrar deneyin. |
| existing | Dependent Variable | Bağımlı Değişken |
| existing | Dependent Variable and Grouping Variable must be distinct columns. | Bağımlı Değişken ve Gruplama Değişkeni birbirinden farklı sütunlar olmalıdır. |
| existing | Display Options | Görüntüleme Seçenekleri |
| existing | Displaying all {pairs} Bayesian pairwise brackets across {groups} groups can crowd the figure; consider setting Pairwise Comparisons to 'None'. | {groups} grup genelinde tüm {pairs} Bayesyen ikili karşılaştırma parantezini göstermek grafiği kalabalıklaştırabilir; İkili Karşılaştırmalar ayarını 'Yok' olarak seçmeyi düşünün. |
| existing | Displaying all {pairs} pairwise brackets across {groups} groups can crowd the figure; consider 'Significant pairs only' or 'None'. | {groups} grup genelinde tüm {pairs} ikili karşılaştırma parantezini göstermek grafiği kalabalıklaştırabilir; 'Yalnızca anlamlı çiftler' veya 'Yok' seçeneğini değerlendirin. |
| existing | Distributions of {outcome} across {groups} groups of {factor} (N = {total}) were displayed using horizontal box-violin plots; formal hypothesis testing via {test} was not estimable ({reason}). | {factor} değişkeninin {groups} grubu genelinde (N = {total}) {outcome} dağılımları yatay kutu-keman grafikleriyle gösterilmiş; {test} ile biçimsel hipotez testi hesaplanamamıştır ({reason}). |
| existing | Each group is drawn as a violin (the shape of its distribution), a boxplot (median and quartiles) and its individual observations. | Her grup bir keman grafiği (dağılımın biçimi), bir kutu grafiği (ortanca ve çeyreklikler) ve bireysel gözlemleriyle birlikte çizilir. |
| existing | Effect Size Measure | Etki Büyüklüğü Ölçütü |
| existing | Enable 'Statistical results in plot' or 'Assumptions and interpretation' to display the selected effect size or confidence level. | Seçilen etki büyüklüğünü veya güven düzeyini görüntülemek için 'Grafikte istatistiksel sonuçlar' ya da 'Varsayımlar ve yorumlama' seçeneğini etkinleştirin. |
| existing | equal relative support for the null and alternative models | sıfır ve alternatif modeller için eşit göreli destek |
| existing | Error | Hata |
| existing | Eta-squared with 3+ groups (Cohen's d with 2 groups) | 3+ grupta eta-kare (2 grupta Cohen's d) |
| existing | Every level of Split By variable '{grvar}' ({levels}) has fewer than two groups or no variation in '{dep}', so no stratified comparison could be computed. | Alt Gruba Göre Böl değişkeni '{grvar}' içindeki her düzeyde ({levels}) ikiden az grup bulunmakta veya '{dep}' değişkeninde değişim bulunmamaktadır; bu nedenle tabakalı karşılaştırma hesaplanamadı. |
| existing | every pair of groups | tüm grup çiftleri |
| existing | For three or more groups with {adjustment} selected, parametric pairwise brackets use pairwise Welch unequal-variance t-tests with {adjustment} adjustment. | Üç veya daha fazla grupta {adjustment} seçildiğinde, parametrik ikili karşılaştırma parantezlerinde {adjustment} düzeltmeli ikili Welch eşit olmayan varyanslı t-testleri kullanılır. |
| existing | For three or more groups, parametric pairwise brackets use the Games-Howell single-step studentized-range procedure for family-wise error control (p_GH-adj); a second adjustment pass is not applied on top of it. | Üç veya daha fazla grup için parametrik ikili karşılaştırma parantezleri, aile bazlı hata kontrolü amacıyla tek adımlı Games-Howell öğrenci-aralığı yöntemini (p_GH-adj) kullanır; bunun üzerine ikinci bir düzeltme uygulanmaz. |
| existing | group '{group}' has fewer than 2 observations (n = {n}) | '{group}' grubunda 2'den az gözlem var (n = {n}) |
| existing | group '{group}' retains fewer than 2 observations after 20 percent trimming | yüzde 20 budama sonrasında '{group}' grubunda 2'den az gözlem kalmaktadır |
| existing | Group summary across N = {total} complete observations: {groupsummary}. | Eksiksiz N = {total} gözlem genelinde grup özeti: {groupsummary}. |
| existing | Grouping Variable | Gruplama Değişkeni |
| existing | Groups do NOT have to share the same spread. | Grupların aynı yayılıma (varyansa) sahip olması GEREKMEZ. |
| existing | Groups do NOT have to share the same spread. Welch's unequal-variance correction is used throughout, which is why the degrees of freedom are fractional. | Grupların aynı yayılıma sahip olması GEREKMEZ. Tüm hesaplamalarda Welch eşit olmayan varyans düzeltmesi kullanılır; serbestlik derecelerinin kesirli olmasının nedeni budur. |
| existing | Groups dropped | Çıkarılan gruplar |
| existing | Groups should share a broadly similar distribution shape if you interpret the result as a median shift; otherwise interpret it as stochastic dominance. | Sonucu ortanca kayması olarak yorumlayacaksanız grupların genel dağılım biçimi benzer olmalıdır; aksi halde sonucu stokastik baskınlık olarak yorumlayın. |
| existing | Hedges' g | Hedges' g |
| existing | Holm | Holm |
| existing | Holm (recommended) | Holm (önerilen) |
| existing | Horizontal Box-Violin Comparison | Yatay Kutu-Keman Karşılaştırması |
| existing | Important | Önemli |
| existing | insufficient variation across groups for Kruskal-Wallis test | Kruskal-Wallis testi için gruplar arasında yeterli değişim yok |
| existing | insufficient within-group variation | grup içi değişim yetersiz |
| added | insufficient within-group variation for Bayesian ANOVA | Bayesyen ANOVA için grup içi değişim yetersiz |
| existing | jamovi (follow global) | jamovi (genel ayarı izle) |
| existing | JJStatsPlotT | JJStatsPlotT |
| existing | Large dataset processed in {seconds} seconds. | Büyük veri kümesi {seconds} saniyede işlendi. |
| existing | Looking for a Cleveland dot chart - one summary point per group, tested against a reference value? Use 'Dot Chart (Summary vs Reference Value)' instead. This analysis uses every observation and compares the groups with each other. | Grup başına tek bir özet noktanın referans değere karşı test edildiği Cleveland nokta grafiğini mi arıyorsunuz? Bunun yerine 'Nokta Grafiği (Özet ve Referans Değer)' analizini kullanın. Bu analiz her gözlemi kullanır ve grupları birbirleriyle karşılaştırır. |
| existing | Mean (average value, sensitive to outliers) | Ortalama (aritmetik ortalama, aykırı değerlere duyarlı) |
| existing | Median (middle value, robust to outliers) | Ortanca (orta değer, aykırı değerlere dayanıklı) |
| existing | Methods sentence you can copy | Kopyalanabilir yöntem metni |
| existing | moderate relative evidence favoring the null hypothesis of no difference | fark olmadığı yönündeki sıfır hipotezi lehine orta düzeyde göreli kanıt |
| existing | moderate relative evidence for a group difference | grup farkı lehine orta düzeyde göreli kanıt |
| existing | Multiple Comparison Adjustment | Çoklu Karşılaştırma Düzeltmesi |
| existing | no | yok |
| existing | No Bayes factor caption is shown because {reason}. | Bayes faktörü alt yazısı gösterilmemektedir ({reason}). |
| existing | No complete observations remain for Split By variable '{grvar}', so the Split By figure cannot be drawn. | Alt Gruba Göre Böl değişkeni '{grvar}' için eksiksiz gözlem kalmadığından bölünmüş grafik çizilemiyor. |
| existing | No normality assumption. The test compares ranks, so it evaluates whether one group tends to yield larger values rather than whether arithmetic means differ. | Normallik varsayımı yoktur. Test sıralamaları karşılaştırır; dolayısıyla aritmetik ortalamaların farklılığından ziyade bir grubun daha büyük değerler üretme eğilimini değerlendirir. |
| existing | No pairwise comparisons were drawn. | İkili karşılaştırma çizilmedi. |
| existing | No post-hoc pairwise comparisons were drawn. | Post-hoc ikili karşılaştırma çizilmedi. |
| existing | None | Yok |
| existing | None (no central line) | Yok (merkez çizgisi yok) |
| existing | None (not recommended) | Yok (önerilmez) |
| existing | Nonparametric (Mann-Whitney U or Kruskal-Wallis) | Parametrik olmayan (Mann-Whitney U veya Kruskal-Wallis) |
| existing | not statistically significant at the 0.05 threshold; absence of significance does not establish equivalence | 0.05 eşiğinde istatistiksel olarak anlamlı değil; anlamlılık saptanmaması eşdeğerlik kanıtlamaz |
| existing | Note | Not |
| existing | Notices | Bildirimler |
| existing | Observations are independent across patients or specimens; clustered or repeated measurements require a paired or mixed-effects rank procedure. | Gözlemler hastalar veya örnekler arasında bağımsızdır; kümelenmiş ya da tekrarlı ölçümler eşleştirilmiş veya karma etkili sıra yöntemleri gerektirir. |
| existing | Observations are independent across patients or specimens; repeated or multi-core measurements from one patient violate independence. | Gözlemler hastalar veya örnekler arasında bağımsızdır; aynı hastadan alınan tekrarlı veya çoklu doku koru ölçümleri bağımsızlığı ihlal eder. |
| existing | Observations are independent across patients or specimens. In small groups, trimming 20 percent from each tail leaves few effective observations - check per-group sizes. | Gözlemler hastalar veya örnekler arasında bağımsızdır. Küçük gruplarda her iki uçtan yüzde 20 budama etkin gözlem sayısını azaltır; grup örneklem büyüklüklerini kontrol edin. |
| existing | Observations are independent. | Gözlemler birbirinden bağımsızdır. |
| existing | Observations are independent. Repeated measurements on the same patient, or multiple cores/blocks from one specimen, violate independence and need a paired or mixed-effects model instead. | Gözlemler birbirinden bağımsızdır. Aynı hastadaki tekrarlı ölçümler veya tek bir örnekten alınan çoklu kor/blok ölçümleri bağımsızlığı ihlal eder; bunun yerine eşleştirilmiş veya karma etkili model gerekir. |
| existing | Okabe-Ito (color-blind safe) | Okabe-Ito (renk körlüğüne uygun) |
| existing | Omega-squared with 3+ groups (Hedges' g with 2 groups) | 3+ grupta omega-kare (2 grupta Hedges' g) |
| existing | omnibus comparison across all groups | tüm gruplar genelindeki genel (omnibus) karşılaştırma |
| existing | ordinal epsilon-squared | ordinal epsilon-kare |
| existing | Ordinal epsilon-squared is accompanied by a two-sided bias-corrected and accelerated (BCa) bootstrap confidence interval across 1,000 resamples. | Ordinal epsilon-kare ölçütüne, 1.000 yeniden örnekleme üzerinden hesaplanan iki yönlü yanlılık düzeltmeli ve hızlandırılmış (BCa) bootstrap güven aralığı eşlik eder. |
| existing | Original ggstatsplot theme | Orijinal ggstatsplot teması |
| existing | Pairwise Comparisons | İkili Karşılaştırmalar |
| existing | Pairwise comparisons are shown without any adjustment for multiple testing. With {groups} groups that is {pairs} comparisons, so even if no groups truly differ there is about a {risk} percent chance that at least one of them reaches significance. Use Holm unless you have a specific reason not to. | İkili karşılaştırmalar çoklu test düzeltmesi yapılmadan gösterilmektedir. {groups} grupta bu durum {pairs} karşılaştırma anlamına gelir; gruplar arasında gerçekte hiçbir fark olmasa bile en az bir karşılaştırmanın anlamlı çıkma olasılığı yaklaşık yüzde {risk} düzeyindedir. Özel bir gerekçeniz yoksa Holm düzeltmesini kullanın. |
| existing | Pairwise comparisons between {which} are shown as brackets using pairwise Welch t-tests with {adjustment} adjustment for multiple testing. | {which} arasındaki ikili karşılaştırmalar, çoklu test düzeltmesi ({adjustment}) ile ikili Welch t-testleri kullanılarak parantezlerle gösterilmiştir. |
| existing | Pairwise comparisons between {which} are shown as brackets using the Games-Howell single-step studentized-range procedure for family-wise error control. | {which} arasındaki ikili karşılaştırmalar, aile bazlı hata kontrolü için tek adımlı Games-Howell öğrenci-aralığı yöntemi kullanılarak parantezlerle gösterilmiştir. |
| existing | Pairwise comparisons between {which} are shown as brackets, with {adjustment} adjustment for multiple testing. | {which} arasındaki ikili karşılaştırmalar, çoklu test düzeltmesi ({adjustment}) ile parantezlerle gösterilmiştir. |
| existing | Pairwise comparisons between every pair of groups are shown as brackets reporting unadjusted natural-log JZS Bayes factors (log_e(BF_01)). | Tüm grup çiftleri arasındaki ikili karşılaştırmalar, düzeltilmemiş doğal logaritmalı JZS Bayes faktörlerini (log_e(BF_01)) raporlayan parantezlerle gösterilmiştir. |
| existing | Parametric (Welch t-test or Welch ANOVA) | Parametrik (Welch t-testi veya Welch ANOVA) |
| existing | partial eta-squared | kısmi eta-kare |
| existing | partial omega-squared | kısmi omega-kare |
| existing | Plot Appearance | Grafik Görünümü |
| existing | Plot Configuration | Grafik Yapılandırması |
| existing | Plot Dimensions | Grafik Boyutları |
| existing | Plot Display | Grafik Gösterimi |
| existing | Plot Height (pixels) | Grafik Yüksekliği (piksel) |
| existing | Plot Labels | Grafik Etiketleri |
| existing | Plot Title | Grafik Başlığı |
| existing | Plot Width (pixels) | Grafik Genişliği (piksel) |
| existing | Post-hoc pairwise brackets were suppressed because the omnibus test could not be evaluated. | Genel (omnibus) test değerlendirilemediğinden post-hoc ikili karşılaştırma parantezleri gizlenmiştir. |
| existing | posterior Bayesian R-squared | sonsal Bayesyen R-kare |
| existing | Potential outliers detected within groups in {name}: {count}. | {name} değişkeninde grup içi olası aykırı değerler saptandı: {count}. |
| existing | Preparing analysis options... | Analiz seçenekleri hazırlanıyor... |
| existing | Processing data for the comparison... | Karşılaştırma için veriler işleniyor... |
| existing | Random seed | Rastgelelik tohumu |
| existing | Reading the figure | Grafiğin okunması |
| existing | Reference Line Value | Referans Çizgisi Değeri |
| existing | Reference Lines | Referans Çizgileri |
| existing | Reference value line | Referans değer çizgisi |
| existing | relative evidence evaluated under the JZS prior | JZS önsel dağılımı altında değerlendirilen göreli kanıt |
| existing | Reproducibility | Tekrarlanabilirlik |
| existing | Result summary | Sonuç özeti |
| existing | Robust (20 percent trimmed means, handles outliers) | Dayanıklı (yüzde 20 budanmış ortalamalar, aykırı değerlere dirençli) |
| existing | Rows excluded because {name} held an infinite value: {count}. Infinite values usually indicate a division by zero or an out-of-range entry - check the source data. | {name} değişkeni sonsuz değer içerdiği için çıkarılan satır sayısı: {count}. Sonsuz değerler genellikle sıfıra bölmeyi veya aralık dışı bir girişi gösterir; kaynak veriyi kontrol edin. |
| existing | Rows excluded for missing values in the analysis variables: {dropped}. | Analiz değişkenlerindeki kayıp değerler nedeniyle çıkarılan satır sayısı: {dropped}. |
| existing | Rows with data: {kept} of {total} ({percent} percent). | Veri içeren satırlar: {kept} / {total} (yüzde {percent}). |
| existing | Select a continuous dependent variable and a categorical grouping variable to begin. | Başlamak için sürekli bir bağımlı değişken ve kategorik bir gruplama değişkeni seçin. |
| existing | Show line (measure taken from Central Tendency Measure) | Çizgiyi göster (ölçüt Merkezi Eğilim Ölçütü ayarından alınır) |
| existing | Significant pairs only | Yalnızca anlamlı çiftler |
| existing | Small subgroup sample sizes (fewer than 10 observations in at least one group) occur within Split By level(s) '{levels}'; interpret per-panel tests cautiously. | Alt Gruba Göre Böl düzey(ler)i '{levels}' içinde küçük alt grup örneklem büyüklükleri (en az bir grupta 10'dan az gözlem) bulunmaktadır; panel bazlı testleri dikkatle yorumlayın. |
| existing | Small total sample size (N = {n}). Statistical tests may be unreliable below 30 observations. Interpret the result cautiously or collect more data. | Toplam örneklem büyüklüğü küçük (N = {n}). 30 gözlemin altında istatistiksel testler güvenilir olmayabilir. Sonucu dikkatle yorumlayın veya daha fazla veri toplayın. |
| existing | Split By (Optional) | Alt Gruba Göre Böl (İsteğe Bağlı) |
| existing | Split By level(s) '{levels}' contain at least one group with fewer than 2 observations, so statistical test subtitles could not be computed for those panel(s). | Alt Gruba Göre Böl düzey(ler)i '{levels}', 2'den az gözleme sahip en az bir grup içerdiğinden bu panel(ler) için istatistiksel test alt başlıkları hesaplanamadı. |
| existing | Split By level(s) '{levels}' had fewer than two groups with data or no variation in '{dep}', so statistical test results could not be computed for those panel(s). | Alt Gruba Göre Böl düzey(ler)i '{levels}' içinde verisi olan grup sayısı ikiden az olduğundan veya '{dep}' değişkeninde değişim bulunmadığından bu panel(ler) için istatistiksel test sonuçları hesaplanamadı. |
| existing | Split By note: {count} observation(s) with missing {grvar} are excluded from the Split By panels only. | Alt Gruba Göre Böl notu: {grvar} değeri eksik olan {count} gözlem yalnızca Alt Gruba Göre Böl panellerinden çıkarılmıştır. |
| existing | Statistical Analysis | İstatistiksel Analiz |
| existing | Statistical Options | İstatistiksel Seçenekler |
| existing | Statistical Precision (Decimal Places) | İstatistiksel Hassasiyet (Ondalık Basamak) |
| existing | Statistical results in plot | Grafikte istatistiksel sonuçlar |
| existing | Statistical test results could not be computed ({reason}), so no test subtitle is shown on the plot. | İstatistiksel test sonuçları hesaplanamadı ({reason}); bu nedenle grafikte test alt başlığı gösterilmemektedir. |
| existing | Statistical Test Type | İstatistiksel Test Türü |
| existing | statistically significant at the 0.05 threshold | 0.05 eşiğinde istatistiksel olarak anlamlı |
| existing | strong relative evidence favoring the null hypothesis of no difference | fark olmadığı yönündeki sıfır hipotezi lehine güçlü göreli kanıt |
| existing | strong relative evidence for a group difference | grup farkı lehine güçlü göreli kanıt |
| existing | Test Method | Test Yöntemi |
| existing | The 'Effect Size Measure' setting does not reach the Split By panels: the statistics package computes one subtitle per panel internally and always uses its own default there. The single unsplit figure below honours your selection. | 'Etki Büyüklüğü Ölçütü' ayarı Alt Gruba Göre Böl panellerine uygulanmaz: istatistik paketi her panel için alt başlığı dahili olarak hesaplar ve orada kendi varsayılanını kullanır. Aşağıdaki bölünmemiş ana grafik seçiminizi uygular. |
| existing | The {comparison} tests whether the groups differ overall, not by how much any one pair differs or whether a difference matters clinically. Read the effect size and its interval alongside the test statistic. | Bu {comparison}, herhangi bir çiftin ne kadar farklılaştığını veya farkın klinik açıdan önemli olup olmadığını değil, grupların genel olarak farklılaşıp farklılaşmadığını test eder. Etki büyüklüğünü ve aralığını test istatistiğiyle birlikte değerlendirin. |
| existing | The Bayes factor caption is omitted on the unsplit figure ({reason}), but is shown on two-group Split By panels. | Bölünmemiş grafikte Bayes faktörü alt yazısı gösterilmemektedir ({reason}); ancak iki gruplu Alt Gruba Göre Böl panellerinde gösterilir. |
| existing | The Bayes factor is computed under a default Jeffreys-Zellner-Siow Cauchy prior on standardized effects (r = 0.707 for two groups; r = 0.5 for three or more groups) assuming a shared residual variance across groups. Treat it as relative evidence under a conventional prior, not as a posterior probability that a hypothesis is true. | Bayes faktörü, gruplar arasında ortak kalıntı varyansı varsayımıyla standartlaştırılmış etkiler üzerinde varsayılan Jeffreys-Zellner-Siow Cauchy önsel dağılımı (iki grup için r = 0.707; üç veya daha fazla grup için r = 0.5) altında hesaplanır. Bunu bir hipotezin doğru olma sonsal olasılığı olarak değil, geleneksel bir önsel dağılım altındaki göreli kanıt olarak değerlendirin. |
| existing | The comparison is horizontal, so values run along the bottom axis and group labels down the side. | Karşılaştırma yatay olarak çizilmiştir; değerler alt eksen boyunca, grup etiketleri ise yan eksen boyunca yer alır. |
| existing | The comparison uses 20 percent trimmed means, discarding the most extreme 20 percent in each tail of every group before testing. Use this when real extreme observations should neither be deleted nor dominate the estimate. | Karşılaştırma yüzde 20 budanmış ortalamaları kullanır ve test öncesinde her grubun her iki ucundaki en aşırı yüzde 20'lik kısmı dışarıda bırakır. Gerçek uç gözlemlerin ne silinmesi ne de tahmini tek başına belirlemesi istendiğinde bu yöntemi kullanın. |
| existing | The confidence interval on the effect size is obtained by bootstrap resampling with a fixed random seed ({seed}) so this figure reproduces exactly; change the seed under Plot Configuration to verify that a borderline interval is stable across resamples. | Etki büyüklüğüne ilişkin güven aralığı, grafiğin birebir tekrarlanabilmesi için sabit bir rastgelelik tohumu ({seed}) kullanılarak bootstrap yeniden örneklemesiyle elde edilir; sınırda kalan bir aralığın yeniden örneklemeler arasında kararlı olduğunu doğrulamak için Grafik Yapılandırması altından tohumu değiştirebilirsiniz. |
| existing | The effect size is estimated from the Welch model rather than an equal-variance ANOVA, and its interval is reported as a one-sided lower confidence bound up to 1.00 (Steiger, 2004). | Etki büyüklüğü eşit varyanslı ANOVA yerine Welch modelinden tahmin edilir ve aralığı 1.00 üst sınırına kadar tek yönlü alt güven sınırı olarak raporlanır (Steiger, 2004). |
| existing | The effect size measure you selected could not be applied ({reason}), so the plot shows the statistics package default instead. | Seçtiğiniz etki büyüklüğü ölçütü uygulanamadı ({reason}); bu nedenle grafikte istatistik paketinin varsayılan ölçütü gösterilmektedir. |
| existing | the explanatory measure of effect size (xi) | açıklayıcı etki büyüklüğü ölçütü (xi) |
| existing | The figure displays log_e(BF_01), the natural logarithm of the Bayes factor favoring the null over the alternative: 0 indicates equal support (BF_01 = 1), negative values favor a group difference (below -1.10 moderate evidence, BF_10 > 3; below -2.30 strong evidence, BF_10 > 10), and positive values (above +1.10 or +2.30) favor the null hypothesis of no difference. | Grafik, sıfır hipotezini alternatif hipoteze karşı destekleyen Bayes faktörünün doğal logaritmasını (log_e(BF_01)) gösterir: 0 eşit desteği (BF_01 = 1), negatif değerler grup farkını (-1.10 altı orta düzey kanıt, BF_10 > 3; -2.30 altı güçlü kanıt, BF_10 > 10), pozitif değerler ise (+1.10 veya +2.30 üstü) fark olmadığı yönündeki sıfır hipotezini destekler. |
| existing | the Kruskal-Wallis test | Kruskal-Wallis testi |
| existing | The Mann-Whitney p value uses the tie-adjusted normal approximation with continuity correction. | Mann-Whitney p-değeri, süreklilik düzeltmeli ve bağlı sıra (tie) uyarlamalı normal yaklaşımı kullanır. |
| existing | the Mann-Whitney U test | Mann-Whitney U testi |
| existing | the pairs that reached significance | anlamlılığa ulaşan grup çiftleri |
| existing | The plot could not be drawn: {reason}. Check the dependent variable for constant values, extreme outliers or too few observations per group, or try a different statistical test. | Grafik çizilemedi: {reason}. Bağımlı değişkende sabit değerler, aşırı uç değerler veya grup başına çok az gözlem olup olmadığını kontrol edin ya da farklı bir istatistiksel test deneyin. |
| existing | the plot is not showing statistical results at all | grafikte istatistiksel sonuçlar kapalıdır |
| existing | the posterior median difference | sonsal ortanca fark |
| existing | the rank-biserial correlation | sıra-iki serili korelasyon |
| existing | the robust (AKP) standardized difference | dayanıklı (AKP) standartlaştırılmış fark |
| existing | the selected effect size | seçilen etki büyüklüğü |
| existing | the selected test | seçilen test |
| existing | The Split By variable '{grvar}' is missing for {count} observation(s); those rows are excluded from the Split By panels only and remain in the main unsplit comparison. | Alt Gruba Göre Böl değişkeni '{grvar}', {count} gözlemde eksiktir; bu satırlar yalnızca Alt Gruba Göre Böl panellerinden çıkarılmış olup bölünmemiş ana karşılaştırmada korunmaktadır. |
| existing | The Split By variable must be distinct from Dependent Variable and Grouping Variable. | Alt Gruba Göre Böl değişkeni, Bağımlı Değişken ve Gruplama Değişkeninden farklı olmalıdır. |
| existing | The split figure could not be drawn: {reason}. Check that every level of the Split By variable has enough data in at least two groups. | Bölünmüş grafik çizilemedi: {reason}. Alt Gruba Göre Böl değişkeninin her düzeyinde en az iki grupta yeterli veri bulunduğunu kontrol edin. |
| existing | the statistics engine returned no Bayes factor | istatistik motoru Bayes faktörü döndürmedi |
| existing | the statistics engine returned no expression | istatistik motoru ifade döndürmedi |
| existing | the statistics package computes one only for a two-group comparison | istatistik paketi bunu yalnızca iki gruplu karşılaştırmalar için hesaplamaktadır |
| existing | the test is already Bayesian, so the subtitle itself reports the Bayes factor | test zaten Bayesyen olduğundan Bayes faktörü doğrudan alt başlıkta raporlanmaktadır |
| existing | the test statistic could not be evaluated (zero within-group spread) | grup içi yayılım sıfır olduğundan test istatistiği hesaplanamadı |
| existing | The values are roughly symmetric within each group. The test tolerates mild skew at moderate sample sizes but not a heavy tail or a floor/ceiling effect - inspect the violins. | Değerler her grup içinde kabaca simetriktir. Test orta büyüklükteki örneklemlerde hafif çarpıklığı tolere eder ancak ağır kuyrukları veya taban/tavan etkisini tolere etmez; keman grafiklerini inceleyin. |
| existing | This analysis compares a continuous variable across groups and draws the comparison horizontally, as a box-violin figure with the individual observations shown. | Bu analiz sürekli bir değişkeni gruplar arasında karşılaştırır ve karşılaştırmayı bireysel gözlemlerin gösterildiği yatay bir kutu-keman grafiği olarak çizer. |
| existing | This analysis is built on the {pkg1} and {pkg2} packages; please cite jamovi and the packages listed below. | Bu analiz {pkg1} ve {pkg2} paketleri üzerine kurulmuştur; lütfen jamovi'yi ve aşağıda listelenen paketleri kaynak gösterin. |
| existing | To Do | Yapılacaklar |
| existing | Trimmed Mean (average excluding extreme values) | Budanmış Ortalama (uç değerler dışlandıktan sonraki ortalama) |
| existing | two-group comparison | iki grup karşılaştırması |
| existing | Uncorrected - Cohen's d with 2 groups, eta-squared with 3+ | Düzeltilmemiş - 2 grupta Cohen's d, 3+ grupta eta-kare |
| existing | Variable '{name}' contains no finite numeric measurements. Please select a valid continuous numeric variable. | '{name}' değişkeni sonlu sayısal ölçüm içermemektedir. Lütfen geçerli bir sürekli sayısal değişken seçin. |
| existing | Variable '{name}' was not found in the data. Available variables are: {available}. Please select a valid continuous variable for the dependent variable. | '{name}' değişkeni veride bulunamadı. Mevcut değişkenler: {available}. Lütfen bağımlı değişken için geçerli bir sürekli değişken seçin. |
| existing | Variable '{name}' was not found in the data. Available variables are: {available}. Please select a valid grouping variable. | '{name}' değişkeni veride bulunamadı. Mevcut değişkenler: {available}. Lütfen geçerli bir gruplama değişkeni seçin. |
| existing | Variable '{name}' was not found in the data. Available variables are: {available}. Please select a valid Split By variable. | '{name}' değişkeni veride bulunamadı. Mevcut değişkenler: {available}. Lütfen geçerli bir Alt Gruba Göre Böl değişkeni seçin. |
| existing | Very small group sizes detected: the smallest is n = {n} in group '{group}'. Groups below 10 observations may produce unreliable test results. Consider combining groups or collecting more data. | Çok küçük grup örneklem büyüklükleri saptandı: en küçüğü '{group}' grubunda n = {n}. 10 gözlemin altındaki gruplar güvenilir olmayan test sonuçları üretebilir. Grupları birleştirmeyi veya daha fazla veri toplamayı düşünün. |
| existing | Viridis (perceptually uniform) | Viridis (algısal olarak tekdüze) |
| existing | Warning | Uyarı |
| existing | Warning: {name} has fewer than 3 valid observations. | Uyarı: {name} değişkeninde 3'ten az geçerli gözlem bulunmaktadır. |
| existing | Warning: {name} has no variation - every value is the same. | Uyarı: {name} değişkeninde değişim yok; tüm değerler aynı. |
| existing | weak relative evidence toward a group difference | grup farkı yönünde zayıf göreli kanıt |
| existing | weak relative evidence toward the null hypothesis of no difference | fark olmadığı yönündeki sıfır hipotezi yönünde zayıf göreli kanıt |
| existing | Welch's one-way analysis of variance | Welch tek yönlü varyans analizi |
| existing | Welch's t-test | Welch t-testi |
| existing | Welcome to ClinicoPath | ClinicoPath'e Hoş Geldiniz |
| existing | What this analysis assumes | Bu analizin varsayımları |
| existing | X-axis Label (Values) | X Ekseni Etiketi (Değerler) |
| existing | Y-axis Label (Groups) | Y Ekseni Etiketi (Gruplar) |
| existing | Your two central-tendency settings disagree: 'Central Tendency Display' still carries the legacy value '{legacy}' while 'Central Tendency Measure' selects {selected}. The plot shows the {drawn}, because 'Central Tendency Display' only decides whether a line is drawn at all. | İki merkezi eğilim ayarınız birbiriyle uyuşmuyor: 'Merkezi Eğilim Gösterimi' eski '{legacy}' değerini taşırken 'Merkezi Eğilim Ölçütü' {selected} seçeneğini belirlemektedir. 'Merkezi Eğilim Gösterimi' yalnızca çizginin çizilip çizilmeyeceğini belirlediğinden grafikte {drawn} gösterilmektedir. |
| existing | Yuen's test reports an unsigned \|t\| statistic, while the robust AKP effect size standardizes the trimmed-mean difference by the first group's Winsorized spread. | Yuen testi işaretsiz \|t\| istatistiğini raporlar; dayanıklı AKP etki büyüklüğü ise budanmış ortalama farkını ilk grubun Winsorize yayılımına göre standartlaştırır. |
| existing | Yuen's trimmed-mean test | Yuen budanmış ortalama testi |
| existing | zero within-group variation | grup içi değişim sıfır |
