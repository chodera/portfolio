---
layout: post
title: "Violent attacks against refugees in Germany"
output:
  html_document:
    df_print: paged
image: assets/images/mut-gegen-rechte-gewalt_logo2.jpg
normal: false
show: false
logo: python
published: true
---

Anti-refugee attacks in the Federal Republic of Germany, including direct verbal and physical crimes against refugees, refugee shelters and facilities for asylum seekers, have strongly increased since the refugee crisis in Europe in 2015. The project [Chronik flüchtlingsfeindlicher Vorfälle](https://www.mut-gegen-rechte-gewalt.de/service/chronik-vorfaelle){:target="_blank"} (*engl. Chronicle of refugee incidents*) documents attacks on and demonstrations against refugees and refugee shelters. It is based on publicly accessible reports in newspaper articles, police press releases and reports from counselling centres for victims of right-wing, racist and anti-Semitic violence.

This post shows how to scrape data from the website, turn it into machine readable format and analyze it (both over time and area). It was inspired by a [similar project](https://vis.one/rechte-gewalt/){:target="_blank"}, which uses regional sources however.

## 1. Scraping "Chronik flüchtlingsfeindlicher Vorfälle"

### Setup

Modules to be loaded are numpy and pandas for handling the data, requests-html for scraping the website, as well as seaborn for plotting.


{% highlight python %}
%matplotlib inline
import numpy as np
import pandas as pd
import seaborn as sns
from requests_html import HTMLSession
{% endhighlight %}

### Prepare the scraper

First thing to do is to establish a html session to the website.


{% highlight python %}
session = HTMLSession()
r = session.get('https://www.mut-gegen-rechte-gewalt.de/service/chronik-vorfaelle')
{% endhighlight %}

Next, we define the urls to be scraped. In total, there are 903 sites, the first one without a suffix, later on with a suffix indicating the site number which makes it straightforward to set the full urls.


{% highlight python %}
# Initialize lists
suffixes = ['']
urls = []

# Set suffix of url
for i in range(1, 902):
    suffixes.append('?page=' + str(i))

# Set full urls
for suffix in suffixes:
    urls.append('https://www.mut-gegen-rechte-gewalt.de/service/chronik-vorfaelle' + suffix)
{% endhighlight %}

Let's print the first five urls to check if it worked out.

{% highlight python %}
print(*urls[0:5], sep='\n')
{% endhighlight %}
{% highlight python %}
https://www.mut-gegen-rechte-gewalt.de/service/chronik-vorfaelle
https://www.mut-gegen-rechte-gewalt.de/service/chronik-vorfaelle?page=1
https://www.mut-gegen-rechte-gewalt.de/service/chronik-vorfaelle?page=2
https://www.mut-gegen-rechte-gewalt.de/service/chronik-vorfaelle?page=3
https://www.mut-gegen-rechte-gewalt.de/service/chronik-vorfaelle?page=4
{% endhighlight %}

The urls look fine, now we ready to launch the scraper. It loops over every entry and scrapes information on each of the respective fields. If there is no information on a variable in a certain entry, the value None is assigned.


{% highlight python %}
data = []

for n in urls:

    r = session.get(n)

    for element in r.html.find('.node-chronik-eintrag'):

        if element.find('.field-name-field-date', first = True) == None:
            date = None
        else:
            date = element.find('.field-name-field-date', first = True).text

        if element.find('.field-name-field-art', first = True) == None:
            category = None
        else:
            category = element.find('.field-name-field-art', first = True).text

        if element.find('.field-name-field-anzahl-verletze', first = True) == None:
            casualties = None
        else:
            casualties = element.find('.field-name-field-anzahl-verletze', first = True).text               

        if element.find('.field-name-field-city', first = True) == None:
            city = None
        else:
            city = element.find('.field-name-field-city', first = True).text

        if element.find('.field-name-field-bundesland', first = True) == None:
            bundesland = None        
        else:
            bundesland = element.find('.field-name-field-bundesland', first = True).text

        if element.find('a[href^="http"]', first = True) == None:
            source = None
        else:
            source = element.find('a[href^="http"]', first = True).text

        if element.find('a[href^="http"]', first = True) == None:
            source_url = None
        else:
            source_url = element.find('a[href^="http"]', first = True).links

        if element.find('.group-right', first = True) == None:
            description = None  
        else:
            description = element.find('.group-right', first = True).text

        data.append({'date': date,
                     'category': category,
                     'casualties': casualties,
                     'city': city,
                     'bundesland': bundesland,
                     'source': source,
                     'source_url': source_url,
                     'description': description})
{% endhighlight %}

Let's print the first three records to take a look at the scraping output.


{% highlight python %}
print(*data[0:3], sep='\n')
{% endhighlight %}

{% highlight python %}
{'date': '17.05.2019', 'category': 'Tätlicher Übergriff/Körperverletzung', 'casualties': '1Verletzte_r', 'city': 'Prenzlau', 'bundesland': 'Brandenburg', 'source': 'Nordkurier', 'source_url': {'https://www.nordkurier.de/uckermark/junge-maenner-in-prenzlau-randalieren-1935542005.html'}, 'description': 'Zwei Deutsche haben am Abend zunächst neben einer Asylunterkunft randaliert. Als Kinder, die in der Asylunterkunft leben, sie aufforderten, dies zu unterlassen, betraten die beiden 21- bzw. 23-Jährigen das Gelände der Unterkunft. Einer von ihnen zückte ein Messer und soll laut Polizei "Stichbewegungen gegen einen tschetschenischen Bewohner ausgeführt haben. Bei der folgenden Rangelei verletzte sich der Tschetschene an der Hand, ein Deutscher erlitt Verletzungen am Bein und musste operiert werden", so die Polizei weiter. Die Kriminalpolizei ermittelt.'}
{'date': '04.05.2019', 'category': 'Tätlicher Übergriff/Körperverletzung', 'casualties': None, 'city': 'Querfurt', 'bundesland': 'Sachsen-Anhalt', 'source': 'Mitteldeutsche Zeitung', 'source_url': {'https://www.mz-web.de/saalekreis/staatsschutz-ermittelt-junger-syrer-rassistisch-beschimpft-und-attackiert-32472432'}, 'description': 'Ein 21-jähriger aus Syrien wurde in der Nacht aus einer Gruppe aus fünf oder sechs jungen Deutschen zunächst rassistisch beleidigt und dann auch geschlagen. Als ein 47-jähriger Zeuge dazwischengehen wollte, sollen ihn die Angreifer zurückgestoßen und am Fuß verletzt haben. Der 21-Jährige musste nicht behandelt werden. Die Täter flüchteten, der Staatsschutz ermittelt.'}
{'date': '01.05.2019', 'category': 'Sonstige Angriffe', 'casualties': None, 'city': 'Kirchheim', 'bundesland': 'Hessen', 'source': 'Süddeutsche Zeitung', 'source_url': {'https://www.sueddeutsche.de/muenchen/staatsschutz-ermittelt-angriffe-auf-fluechtlinge-und-ein-drohbrief-1.4429977'}, 'description': 'Unbekannte haben in der Nacht Eier gegen die Fassade einer Asylunterkunft geworfen. Als ein Mitarbeiter des Sicherheitsdienstes die Täter ansprach, flüchteten sie.'}
{% endhighlight %}


### Turn into a dataframe and save as csv file

We use pandas methods to turn the data records into a data frame and to save it as a csv file:


{% highlight python %}
cols = ['date', 'category', 'city', 'bundesland', 'casualties', 'description', 'source', 'source_url']
df = pd.DataFrame.from_records(data, columns=cols)
df.to_csv('data/mut_gegen_rechte_gewalt.csv', index=False)
{% endhighlight %}

## 2. Data wrangling and exploration

Get a first overview of the data.


{% highlight python %}
df.info()
{% endhighlight %}

{% highlight python %}
<class 'pandas.core.frame.DataFrame'>
RangeIndex: 9012 entries, 0 to 9011
Data columns (total 8 columns):
date           9012 non-null object
category       9012 non-null object
city           9012 non-null object
bundesland     9012 non-null object
casualties     611 non-null object
description    9012 non-null object
source         8258 non-null object
source_url     8259 non-null object
dtypes: object(8)
memory usage: 563.3+ KB
{% endhighlight %}


{% highlight python %}
df.head()
{% endhighlight %}


<div style="overflow-x:auto;">
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>date</th>
      <th>category</th>
      <th>city</th>
      <th>bundesland</th>
      <th>casualties</th>
      <th>description</th>
      <th>source</th>
      <th>source_url</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>17.05.2019</td>
      <td>Tätlicher Übergriff/Körperverletzung</td>
      <td>Prenzlau</td>
      <td>Brandenburg</td>
      <td>1Verletzte_r</td>
      <td>Zwei Deutsche haben am Abend zunächst neben ei...</td>
      <td>Nordkurier</td>
      <td>{'https://www.nordkurier.de/uckermark/junge-ma...</td>
    </tr>
    <tr>
      <th>1</th>
      <td>04.05.2019</td>
      <td>Tätlicher Übergriff/Körperverletzung</td>
      <td>Querfurt</td>
      <td>Sachsen-Anhalt</td>
      <td>NaN</td>
      <td>Ein 21-jähriger aus Syrien wurde in der Nacht ...</td>
      <td>Mitteldeutsche Zeitung</td>
      <td>{'https://www.mz-web.de/saalekreis/staatsschut...</td>
    </tr>
    <tr>
      <th>2</th>
      <td>01.05.2019</td>
      <td>Sonstige Angriffe</td>
      <td>Kirchheim</td>
      <td>Hessen</td>
      <td>NaN</td>
      <td>Unbekannte haben in der Nacht Eier gegen die F...</td>
      <td>Süddeutsche Zeitung</td>
      <td>{'https://www.sueddeutsche.de/muenchen/staatss...</td>
    </tr>
    <tr>
      <th>3</th>
      <td>31.03.2019</td>
      <td>Verdachtsfall</td>
      <td>Lübeck</td>
      <td>Schleswig-Holstein</td>
      <td>1Verletzte_r</td>
      <td>Zwei unbekannte Männer haben einen 27-jährigen...</td>
      <td>n-tv</td>
      <td>{'https://www.n-tv.de/regionales/hamburg-und-s...</td>
    </tr>
    <tr>
      <th>4</th>
      <td>02.03.2019</td>
      <td>Tätlicher Übergriff/Körperverletzung</td>
      <td>Leipzig</td>
      <td>Sachsen</td>
      <td>1Verletzte_r</td>
      <td>Eine Gruppe von acht Männern hat am Nachmittag...</td>
      <td>Peiner Allgemeine</td>
      <td>{'http://www.paz-online.de/Nachrichten/Panoram...</td>
    </tr>
  </tbody>
</table>
</div>

<br>

### Data cleaning


{% highlight python %}
df.dtypes
{% endhighlight %}

{% highlight python %}
date           object
category       object
city           object
bundesland     object
casualties     object
description    object
source         object
source_url     object
dtype: object
{% endhighlight %}

All columns are of the type "object". The least we should do is to turn the date column into a datetime object. In the following steps, each column will be inspected and treated one by one.

#### Date

Change date column to datetime object.


{% highlight python %}
df['date'] = pd.to_datetime(df['date'], format='%d.%m.%Y')
{% endhighlight %}

#### Category


{% highlight python %}
df['category'].value_counts()
{% endhighlight %}

{% highlight python %}
Sonstige Angriffe                       6611
Tätlicher Übergriff/Körperverletzung    1482
Kundgebung/Demo                          361
Verdachtsfall                            286
Brandanschlag                            272
Name: category, dtype: int64
{% endhighlight %}


As the data has been scraped from a German website, the five category names are in German. We will replace the German categories with the English translation.


{% highlight python %}
mapping_dict = {
    'Tätlicher Übergriff/Körperverletzung': 'Assault and battery',
    'Brandanschlag': 'Arson attack',
    'Kundgebung/Demo': 'Rally/demonstration',
    'Sonstige Angriffe': 'Other attacks',
    'Verdachtsfall': 'Suspected case'
}
df['category'] = df['category'].map(mapping_dict)
df['category'].value_counts()
{% endhighlight %}

#### City


{% highlight python %}
df['city'].unique().shape[0]
{% endhighlight %}

{% highlight python %}
2505
{% endhighlight %}

There are 2505 unique places in Germany where an attack on refugees occured.


{% highlight python %}
df['city'].unique()[0:20]
{% endhighlight %}

{% highlight python %}
array(['Prenzlau', 'Querfurt', 'Kirchheim', 'Lübeck', 'Leipzig',
           'Ahrensburg', 'Marzahn, Berlin', 'Zittau', 'Mühlhausen',
           'Plattling', 'Stralsund', 'Hebsack, Remshalden',
           'Vaihingen an der Enz', 'Lütten-Klein, Rostock', 'Spremberg',
           'Neubrandenburg', 'Bad Oeynhausen', 'Cottbus', 'Düsseldorf',
           'Magdeburg'], dtype=object)
{% endhighlight %}


#### Bundesland


{% highlight python %}
df['bundesland'].unique().shape[0]
{% endhighlight %}

{% highlight python %}
16
{% endhighlight %}



{% highlight python %}
df['bundesland'].unique()
{% endhighlight %}

{% highlight python %}
array(['Brandenburg', 'Sachsen-Anhalt', 'Hessen', 'Schleswig-Holstein',
           'Sachsen', 'Berlin', 'Thüringen', 'Bayern',
           'Mecklenburg-Vorpommern', 'Baden-Württemberg',
           'Nordrhein-Westfalen', 'Niedersachsen', 'Rheinland-Pfalz',
           'Saarland', 'Hamburg', 'Bremen'], dtype=object)
{% endhighlight %}


#### Casualties


{% highlight python %}
df['casualties'].head()
{% endhighlight %}

{% highlight python %}
0    1Verletzte_r
1             NaN
2             NaN
3    1Verletzte_r
4    1Verletzte_r
Name: casualties, dtype: object
{% endhighlight %}


The information is inherently numeric but it is concatenated as a string representation. Let's replace the string representation into numerical values by deleting the characters.


{% highlight python %}
df['casualties'] = df['casualties'].str.replace(r' ?(Verletzte)(_r)?', '').astype(float)
df['casualties'].head()
{% endhighlight %}

How many entries actually do contain casualty numbers?


{% highlight python %}
df[df['casualties'].notnull()].shape[0]
{% endhighlight %}

{% highlight python %}
611
{% endhighlight %}


There are only 611 entries which contain information on the number of casualties. I suppose this can be interpreted than in other cases there were (luckily) no casualties.

#### Description


{% highlight python %}
print(*df['description'].head(3), sep='\n\n')
{% endhighlight %}

{% highlight python %}
Zwei Deutsche haben am Abend zunächst neben einer Asylunterkunft randaliert. Als Kinder, die in der Asylunterkunft leben, sie aufforderten, dies zu unterlassen, betraten die beiden 21- bzw. 23-Jährigen das Gelände der Unterkunft. Einer von ihnen zückte ein Messer und soll laut Polizei "Stichbewegungen gegen einen tschetschenischen Bewohner ausgeführt haben. Bei der folgenden Rangelei verletzte sich der Tschetschene an der Hand, ein Deutscher erlitt Verletzungen am Bein und musste operiert werden", so die Polizei weiter. Die Kriminalpolizei ermittelt.

Ein 21-jähriger aus Syrien wurde in der Nacht aus einer Gruppe aus fünf oder sechs jungen Deutschen zunächst rassistisch beleidigt und dann auch geschlagen. Als ein 47-jähriger Zeuge dazwischengehen wollte, sollen ihn die Angreifer zurückgestoßen und am Fuß verletzt haben. Der 21-Jährige musste nicht behandelt werden. Die Täter flüchteten, der Staatsschutz ermittelt.

Unbekannte haben in der Nacht Eier gegen die Fassade einer Asylunterkunft geworfen. Als ein Mitarbeiter des Sicherheitsdienstes die Täter ansprach, flüchteten sie.
{% endhighlight %}

#### Source


{% highlight python %}
df['source'].unique().shape[0]
{% endhighlight %}

{% highlight python %}
596
{% endhighlight %}

{% highlight python %}
df['source'].value_counts()[0:10]
{% endhighlight %}

{% highlight python %}
Antwort auf eine Kleine Anfrage im Bundestag (Drucksache 18/11298)          1957
Antwort auf eine Kleine Anfrage im Bundestag (Drucksache 18/10213)           762
Bundesregierung                                                              662
Antwort der Bundesregierung (Drucksache 19/144)                              478
Antwort der Bundesregierung (Drucksache 19/146)                              417
Antwort der Bundesregierung auf eine Kleine Anfrage (Drucksache 19/889)      352
Antwort der Bundesregierung auf eine Kleine Anfrage (Drucksache 19/3753)     329
Antwort der Bundesregierung auf eine Kleine Anfrage (Drucksache 19/5516)     324
Antwort der Bundesregierung auf eine Kleine Anfrage (Drucksache 19/2490)     315
Antwort auf eine Kleine Anfrage im Bundestag (Drucksache 19/889)             234
Name: source, dtype: int64
{% endhighlight %}


There are 596 different sources, but the most common ones are related to answers from the Bundesregierung. A further inspection reveals that events documented are quite common, too. Therefore we will categorize the variable source into three categories: government, police, others.


{% highlight python %}
# Initialise column
df['source_category'] = df['source']

# Replace null values by empty string (otherwise boolean indexing will throw an arrow because of boolean indexing)
df.loc[pd.isnull(df['source_category']), 'source_category'] = ''

# Replace respective values
df.loc[df['source_category'].str.contains(r'Anfrage|Bundesregierung'), 'source_category'] = 'government'
df.loc[df['source_category'].str.contains(r'[Pp]olizei'), 'source_category'] = 'police'
df.loc[~df['source_category'].str.contains(r'government|police|^$'), 'source_category'] = 'other'

# Replace null values by empty string
df.loc[df['source_category'] == '', 'source_category'] = ''
{% endhighlight %}


{% highlight python %}
df['source_category'].value_counts(dropna = False)
{% endhighlight %}

{% highlight python %}
government    6287
other         1683
               754
police         288
Name: source_category, dtype: int64
{% endhighlight %}


Out of all sources, the vast amount belong to governmental reports (6,287), and a considerate amount to police reports (288). The category "other" includes mostly media and NGOs. Around 10% of the events do not contain information on the source.

#### Source url


{% highlight python %}
print(*df['source_url'].head(), sep='\n')
{% endhighlight %}

{% highlight python %}
{'https://www.nordkurier.de/uckermark/junge-maenner-in-prenzlau-randalieren-1935542005.html'}
{'https://www.mz-web.de/saalekreis/staatsschutz-ermittelt-junger-syrer-rassistisch-beschimpft-und-attackiert-32472432'}
{'https://www.sueddeutsche.de/muenchen/staatsschutz-ermittelt-angriffe-auf-fluechtlinge-und-ein-drohbrief-1.4429977'}
{'https://www.n-tv.de/regionales/hamburg-und-schleswig-holstein/Syrer-mit-Glasflasche-attackiert-Fremdenfeindliches-Motiv-article20957339.html'}
{'http://www.paz-online.de/Nachrichten/Panorama/Auslaenderfeindlicher-Attacke-Acht-betrunkene-Maenner-verpruegeln-Asylbewerber'}
{% endhighlight %}

Delete the curly brackets:

{% highlight python %}
df['source_url'] = df['source_url'].str.replace(r"(\{')?('\})?", '')
print(*df['source_url'].head(), sep='\n')
{% endhighlight %}

### Final checkup

{% highlight python %}
df.info()
{% endhighlight %}

{% highlight python %}
<class 'pandas.core.frame.DataFrame'>
RangeIndex: 9012 entries, 0 to 9011
Data columns (total 9 columns):
date               9012 non-null datetime64[ns]
category           9012 non-null object
city               9012 non-null object
bundesland         9012 non-null object
casualties         611 non-null float64
description        9012 non-null object
source             8258 non-null object
source_url         8259 non-null object
source_category    9012 non-null object
dtypes: datetime64[ns](1), float64(1), object(7)
memory usage: 633.7+ KB
{% endhighlight %}


{% highlight python %}
df.dtypes
{% endhighlight %}

{% highlight python %}
date               datetime64[ns]
category                   object
city                       object
bundesland                 object
casualties                float64
description                object
source                     object
source_url                 object
source_category            object
dtype: object
{% endhighlight %}



{% highlight python %}
df.head()
{% endhighlight %}

<div style="overflow-x:auto;">
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>date</th>
      <th>category</th>
      <th>city</th>
      <th>bundesland</th>
      <th>casualties</th>
      <th>description</th>
      <th>source</th>
      <th>source_url</th>
      <th>source_category</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>2019-05-17</td>
      <td>Assault and battery</td>
      <td>Prenzlau</td>
      <td>Brandenburg</td>
      <td>1.0</td>
      <td>Zwei Deutsche haben am Abend zunächst neben ei...</td>
      <td>Nordkurier</td>
      <td>https://www.nordkurier.de/uckermark/junge-maen...</td>
      <td>other</td>
    </tr>
    <tr>
      <th>1</th>
      <td>2019-05-04</td>
      <td>Assault and battery</td>
      <td>Querfurt</td>
      <td>Sachsen-Anhalt</td>
      <td>NaN</td>
      <td>Ein 21-jähriger aus Syrien wurde in der Nacht ...</td>
      <td>Mitteldeutsche Zeitung</td>
      <td>https://www.mz-web.de/saalekreis/staatsschutz-...</td>
      <td>other</td>
    </tr>
    <tr>
      <th>2</th>
      <td>2019-05-01</td>
      <td>Other attacks</td>
      <td>Kirchheim</td>
      <td>Hessen</td>
      <td>NaN</td>
      <td>Unbekannte haben in der Nacht Eier gegen die F...</td>
      <td>Süddeutsche Zeitung</td>
      <td>https://www.sueddeutsche.de/muenchen/staatssch...</td>
      <td>other</td>
    </tr>
    <tr>
      <th>3</th>
      <td>2019-03-31</td>
      <td>Suspected case</td>
      <td>Lübeck</td>
      <td>Schleswig-Holstein</td>
      <td>1.0</td>
      <td>Zwei unbekannte Männer haben einen 27-jährigen...</td>
      <td>n-tv</td>
      <td>https://www.n-tv.de/regionales/hamburg-und-sch...</td>
      <td>other</td>
    </tr>
    <tr>
      <th>4</th>
      <td>2019-03-02</td>
      <td>Assault and battery</td>
      <td>Leipzig</td>
      <td>Sachsen</td>
      <td>1.0</td>
      <td>Eine Gruppe von acht Männern hat am Nachmittag...</td>
      <td>Peiner Allgemeine</td>
      <td>http://www.paz-online.de/Nachrichten/Panorama/...</td>
      <td>other</td>
    </tr>
  </tbody>
</table>
</div>

<br>

### Additional data

#### Population by Bundesland (for standardization)

In order to standardize the absolute number of attacks in a Bundesland by the population, we will merge official data from the Federal Statistical Office of Germany (source: https://www-genesis.destatis.de/genesis/online).


{% highlight python %}
pop = pd.read_csv('data/12411-0010.csv',
                 sep=";",
                 skiprows=6,
                 nrows=16,
                 header=None,
                 encoding="cp1250",
                 names=['bundesland', 'population'])
df.sort_values('bundesland')['bundesland'].unique()
{% endhighlight %}


{% highlight python %}
pop['bundesland']
{% endhighlight %}

{% highlight python %}
0          Baden-Württemberg
1                     Bayern
2                     Berlin
3                Brandenburg
4                     Bremen
5                    Hamburg
6                     Hessen
7     Mecklenburg-Vorpommern
8              Niedersachsen
9        Nordrhein-Westfalen
10           Rheinland-Pfalz
11                  Saarland
12                   Sachsen
13            Sachsen-Anhalt
14        Schleswig-Holstein
15                 Thüringen
Name: bundesland, dtype: object
{% endhighlight %}



{% highlight python %}
df = df.merge(pop, on='bundesland', how='left')
df.info()
{% endhighlight %}

#### Official statistics on numbers of refugees

Furthermore, official statistics on the monthly number of refugees are included in the analysis. These numbers are published by the Federal Office for Migration and Refugees, unfortunately in PDF format. However, a local initiative from Munich turns these documents into machine readable csv documents and makes them available online: https://github.com/muc-fluechtlingsrat/bamf-asylgeschaeftsstatistik. Moreover, they via the Freedom of Information Act they got access to data from before 2017 which was not published by the Federal Office for Migration and Refugees.

I obtained the data needed for this analysis with an R script (get it here). Here, we only need to load the data:


{% highlight python %}
statistik = pd.read_csv('data/asylmonatszahlen.csv')
statistik.head()
{% endhighlight %}

The steps of preparing this data set for analysis are done in the following section.

## 3. Analyzing the data

### Attacks over time

First, let's prepare the data from the "Chronik": aggregate by month (setting date as index) and count number of attacks.


{% highlight python %}
df['date'] = pd.to_datetime(df['date'])
df_date = df.resample('M', on='date')[['date']].count()
df_date.index = df_date.index.strftime('%b %Y')
df_date.columns = ['n_attacks']
df_date.head()
{% endhighlight %}

<table border="0">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>n_attacks</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>Jan 2015</th>
      <td>78</td>
    </tr>
    <tr>
      <th>Feb 2015</th>
      <td>52</td>
    </tr>
    <tr>
      <th>Mar 2015</th>
      <td>77</td>
    </tr>
    <tr>
      <th>Apr 2015</th>
      <td>69</td>
    </tr>
    <tr>
      <th>May 2015</th>
      <td>76</td>
    </tr>
  </tbody>
</table>

<br>


Prepare the data on monthly number of refugees by formatting it like above.


{% highlight python %}
statistik['date'] = pd.to_datetime(statistik['date'])
statistik = statistik.resample('M', on='date').sum()
statistik.index = statistik.index.strftime('%b %Y')
statistik.columns = ['n_refugees']
statistik.head()
{% endhighlight %}




<div style="overflow-x:auto;">
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>n_refugees</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>Jan 2015</th>
      <td>25042</td>
    </tr>
    <tr>
      <th>Feb 2015</th>
      <td>26083</td>
    </tr>
    <tr>
      <th>Mar 2015</th>
      <td>32054</td>
    </tr>
    <tr>
      <th>Apr 2015</th>
      <td>27178</td>
    </tr>
    <tr>
      <th>May 2015</th>
      <td>25992</td>
    </tr>
  </tbody>
</table>
</div>

<br>

Merge both data sets:


{% highlight python %}
combined = df_date.merge(statistik, how='left', left_index=True, right_index=True)
combined = combined.dropna()
combined.head()
{% endhighlight %}




<div style="overflow-x:auto;">
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>n_attacks</th>
      <th>n_refugees</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>Jan 2015</th>
      <td>78</td>
      <td>25042.0</td>
    </tr>
    <tr>
      <th>Feb 2015</th>
      <td>52</td>
      <td>26083.0</td>
    </tr>
    <tr>
      <th>Mar 2015</th>
      <td>77</td>
      <td>32054.0</td>
    </tr>
    <tr>
      <th>Apr 2015</th>
      <td>69</td>
      <td>27178.0</td>
    </tr>
    <tr>
      <th>May 2015</th>
      <td>76</td>
      <td>25992.0</td>
    </tr>
  </tbody>
</table>
</div>

<br>

Finally, we are ready to plot the data:


{% highlight python %}
# Set figure and font size
plt.figure(figsize=(11, 7))
plt.rcParams.update({'font.size': 14})
# Define plot
ax1 = combined['n_attacks'].plot(color = '#0173b2')
ax2 = plt.twinx()
combined['n_refugees'].plot(color='#de8f05', ax=ax2)
# Set grid lines
ax1.xaxis.grid(True)
ax1.yaxis.grid(True)
# Set labels
plt.title('')
ax1.set_ylabel('Number of attacks')
ax2.set_ylabel('Number of refugees')
# Turn axis ticks off
ax1.tick_params(axis=u'both', which=u'both',length=0)
ax2.tick_params(axis=u'both', which=u'both',length=0)
# Set legend
ax1.set_label('lkmkl')
legend = ax1.figure.legend(loc='upper right', bbox_to_anchor=(0.7, 0.7))
legend.get_texts()[0].set_text('Attacks')
legend.get_texts()[1].set_text('Refugees')
# Avoid clipping of right y labels
plt.tight_layout()
# Remove frame
sns.despine(top=True, right=True, left=True, bottom=True);
{% endhighlight %}

{:refdef: style="text-align: center;"}
![png](../assets/images/attacks_time.png)
{:refdef}

### Attacks by Bundesland

Group the number of attacks by Bundesland:


{% highlight python %}
df_bundesland = df.groupby('bundesland').size().reset_index(name='n').sort_values('n', ascending=False)
df_bundesland
{% endhighlight %}




<div style="overflow-x:auto;">
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>bundesland</th>
      <th>n</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>12</th>
      <td>Sachsen</td>
      <td>1406</td>
    </tr>
    <tr>
      <th>1</th>
      <td>Bayern</td>
      <td>1057</td>
    </tr>
    <tr>
      <th>9</th>
      <td>Nordrhein-Westfalen</td>
      <td>968</td>
    </tr>
    <tr>
      <th>3</th>
      <td>Brandenburg</td>
      <td>930</td>
    </tr>
    <tr>
      <th>0</th>
      <td>Baden-Württemberg</td>
      <td>803</td>
    </tr>
    <tr>
      <th>2</th>
      <td>Berlin</td>
      <td>701</td>
    </tr>
    <tr>
      <th>8</th>
      <td>Niedersachsen</td>
      <td>642</td>
    </tr>
    <tr>
      <th>13</th>
      <td>Sachsen-Anhalt</td>
      <td>539</td>
    </tr>
    <tr>
      <th>15</th>
      <td>Thüringen</td>
      <td>486</td>
    </tr>
    <tr>
      <th>14</th>
      <td>Schleswig-Holstein</td>
      <td>398</td>
    </tr>
    <tr>
      <th>7</th>
      <td>Mecklenburg-Vorpommern</td>
      <td>388</td>
    </tr>
    <tr>
      <th>6</th>
      <td>Hessen</td>
      <td>251</td>
    </tr>
    <tr>
      <th>10</th>
      <td>Rheinland-Pfalz</td>
      <td>228</td>
    </tr>
    <tr>
      <th>5</th>
      <td>Hamburg</td>
      <td>121</td>
    </tr>
    <tr>
      <th>11</th>
      <td>Saarland</td>
      <td>69</td>
    </tr>
    <tr>
      <th>4</th>
      <td>Bremen</td>
      <td>25</td>
    </tr>
  </tbody>
</table>
</div>

<br>

Create a variable indicating if a Bundesland is in Eastern or Western Germany (for coloring in plot):


{% highlight python %}
east = ['Brandenburg', 'Sachsen', 'Mecklenburg-Vorpommern', 'Sachsen-Anhalt', 'Thüringen', 'Berlin']
df_bundesland['east_west'] = df_bundesland['bundesland'].apply(lambda x: 'East' if x in east else 'West')
df_bundesland['east_west'].value_counts()
{% endhighlight %}

{% highlight python %}
West    10
East     6
Name: east_west, dtype: int64
{% endhighlight %}



{% highlight python %}
# Set figure and font size
plt.figure(figsize=(11, 7))
plt.rcParams.update({'font.size': 14})
# Define plot
ax = plt.axes()
sns.barplot(x='n', y='bundesland', data=df_bundesland, hue='east_west', dodge=False)
# Set grid lines
ax.xaxis.grid(True)
# Set labels
plt.title('Absolute number of attacks per Bundesland')
plt.xlabel('')
plt.ylabel('')
# Remove legend title
plt.legend(title='')
# Turn axis ticks off
ax.tick_params(axis=u'both', which=u'both',length=0)
# Remove y axis ticks
ax.tick_params(left=False)
# Remove frame
sns.despine(top=True, right=True, left=True, bottom=True);
{% endhighlight %}

{:refdef: style="text-align: center;"}
![png](../assets/images/attacks_bar_absolute.png)
{:refdef}

### Attacks by Bundesland (standardized by population)

Compute standardized attack rate (100000 attacks per person):


{% highlight python %}
pop_bundesland = df.groupby('population', as_index=False).first()[['bundesland', 'population']]
df_bundesland = df_bundesland.merge(pop_bundesland, on='bundesland', how='left')
df_bundesland['n_std'] = df_bundesland['n'] * 100000 / df_bundesland['population']
df_bundesland = df_bundesland.sort_values('n_std', ascending=False)
{% endhighlight %}


{% highlight python %}
# Set figure and font size
plt.figure(figsize=(11, 7))
plt.rcParams.update({'font.size': 14})
# Define plot
ax = plt.axes()
sns.barplot(x='n_std', y='bundesland', data=df_bundesland, hue='east_west', dodge=False)
# Set grid lines
ax.xaxis.grid(True)
# Set labels
plt.title('Standardized attack rate per Bundesland')
plt.xlabel('100,000 attacks per person')
plt.ylabel('')
# Remove legend title
plt.legend(title='')
# Turn axis ticks off
ax.tick_params(axis=u'both', which=u'both',length=0)
# Remove y axis ticks
ax.tick_params(left=False)
# Remove frame
sns.despine(top=True, right=True, left=True, bottom=True);
{% endhighlight %}

{:refdef: style="text-align: center;"}
![png](../assets/images/attacks_bar_std.png)
{:refdef}

### Attacks by category


{% highlight python %}
df_category = df.groupby('category').size().reset_index(name='n').sort_values('n', ascending=False)
df_category
{% endhighlight %}




<div style="overflow-x:auto;">
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>category</th>
      <th>n</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>2</th>
      <td>Other attacks</td>
      <td>6611</td>
    </tr>
    <tr>
      <th>1</th>
      <td>Assault and battery</td>
      <td>1482</td>
    </tr>
    <tr>
      <th>3</th>
      <td>Rally/demonstration</td>
      <td>361</td>
    </tr>
    <tr>
      <th>4</th>
      <td>Suspected case</td>
      <td>286</td>
    </tr>
    <tr>
      <th>0</th>
      <td>Arson attack</td>
      <td>272</td>
    </tr>
  </tbody>
</table>
</div>

<br>


{% highlight python %}
# Set figure and font size
plt.figure(figsize=(11, 7))
plt.rcParams.update({'font.size': 14})
# Define plot
ax = plt.axes()
sns.barplot(x='n', y='category', data=df_category, color='#0173b2')
# Set grid lines
ax.xaxis.grid(True)
# Set labels
plt.title('Attacks by category')
plt.xlabel('')
plt.ylabel('')
# Turn axis ticks off
ax.tick_params(axis=u'both', which=u'both',length=0)
# Remove frame
sns.despine(top=True, right=True, left=True, bottom=True);
{% endhighlight %}

{:refdef: style="text-align: center;"}
![png](../assets/images/attacks_bar_category.png)
{:refdef}

### Number of casualties

Absolute numbers:

{% highlight python %}
df['casualties'].value_counts(dropna=False)
{% endhighlight %}

{% highlight python %}
NaN     8401
1.0      473
2.0       88
3.0       27
4.0       12
6.0        3
5.0        3
9.0        1
35.0       1
7.0        1
20.0       1
14.0       1
Name: casualties, dtype: int64
{% endhighlight %}


Relative numbers:


{% highlight python %}
df['casualties'].value_counts(normalize=True, dropna=False)
{% endhighlight %}

{% highlight python %}
NaN     0.932202
1.0     0.052486
2.0     0.009765
3.0     0.002996
4.0     0.001332
6.0     0.000333
5.0     0.000333
9.0     0.000111
35.0    0.000111
7.0     0.000111
20.0    0.000111
14.0    0.000111
Name: casualties, dtype: float64
{% endhighlight %}


By far most of the events do not involve any casualties (more than 90%). In around 5% of the events, 1 Person was injured. There are as well three events, in which more than 10 people suffered from injuries. Let's check which event correspond to the event with the highest number of casualties:

{% highlight python %}
print(*df[df['casualties'] == 35]['description'])
{% endhighlight %}

## Conclusion

...

Get the full code [here](https://github.com/chodera/chodera.github.io/tree/master/assets/projects/7_violent_attacks_against_refugees_germany){:target="_blank"}.
