#!/usr/bin/env python3
"""
Centralized country mapping and detection utility.
Used by multiple scripts for consistent country name standardization.
"""

import re

# Comprehensive list of country names, territories, and common aliases
COUNTRIES_AND_ALIASES = {
    'china': ['china', "p.r. china", "pr china", 'peoples republic', 'p.r.c'],
    'united states': ['usa', 'us', 'u.s.', 'united states', 'u.s.a.', 'america'],
    'united kingdom': ['uk', 'u.k.', 'united kingdom', 'great britain', 'britain', 'england'],
    'south korea': ['korea', 'south korea', 'republic of korea', 'r.o.k'],
    'north korea': ['north korea', "dpr korea", 'democratic peoples republic'],
    'iran': ['iran', 'persia', 'islamic republic of iran'],
    'vietnam': ['vietnam', 'viet nam'],
    'new zealand': ['new zealand', 'new zeland', 'aotearoa'],
    'costa rica': ['costa rica', 'costarica'],
    'puerto rico': ['puerto rico', 'puerterico'],
    'sri lanka': ['sri lanka', 'srilanka', 'ceylon'],
    'saudi arabia': ['saudi arabia', 'saudiarabia'],
    'united arab emirates': ['uae', 'u.a.e.', 'united arab emirates', 'emirates'],
    'south africa': ['south africa', 'southafrica', 'republic of south africa'],
    'czech republic': ['czech', 'czech republic', 'czechia'],
    'dominican republic': ['dominican', 'dominican republic'],
    'congo': ['congo', 'drc', 'democratic republic of congo', 'republic of congo'],
    'turkey': ['turkey', 'türkiye', 'turkiye'],
    'russia': ['russia', 'russian federation', 'ussr', 'soviet union'],
    'mexico': ['mexico', 'méxico', 'mexico city'],
    'brazil': ['brazil', 'brasil'],
    'australia': ['australia', 'australian'],
    'india': ['india', 'indian'],
    'japan': ['japan', 'japanese'],
    'france': ['france', 'french'],
    'germany': ['germany', 'german', 'deutschland'],
    'spain': ['spain', 'spanish', 'españa'],
    'italy': ['italy', 'italian', 'italia'],
    'thailand': ['thailand', 'thai'],
    'singapore': ['singapore'],
    'philippines': ['philippines', 'philippine'],
    'indonesia': ['indonesia', 'indonesian'],
    'malaysia': ['malaysia', 'malaysian'],
    'kenya': ['kenya', 'kenyan'],
    'cameroon': ['cameroon', 'cameroun'],
    'colombia': ['colombia', 'colombian'],
    'peru': ['peru', 'perú', 'peruvian'],
    'argentina': ['argentina', 'argentine'],
    'chile': ['chile', 'chilean'],
    'ecuador': ['ecuador', 'ecuadorian'],
    'panama': ['panama', 'panamanian'],
    'guatemala': ['guatemala', 'guatemalan'],
    'honduras': ['honduras', 'honduran'],
    'el salvador': ['el salvador', 'salvador'],
    'nicaragua': ['nicaragua', 'nicaraguan'],
    'belize': ['belize', 'belizean'],
    'jamaica': ['jamaica', 'jamaican'],
    'haiti': ['haiti', 'haitian'],
    'cuba': ['cuba', 'cuban'],
    'canada': ['canada', 'canadian'],
    'greenland': ['greenland', 'groenland'],
    'iceland': ['iceland', 'icelandic'],
    'norway': ['norway', 'norwegian'],
    'sweden': ['sweden', 'swedish'],
    'finland': ['finland', 'finnish'],
    'denmark': ['denmark', 'danish'],
    'belgium': ['belgium', 'belgian'],
    'netherlands': ['netherlands', 'dutch', 'holland'],
    'switzerland': ['switzerland', 'swiss', 'helvetia'],
    'austria': ['austria', 'austrian'],
    'poland': ['poland', 'polish'],
    'portugal': ['portugal', 'portuguese'],
    'greece': ['greece', 'greek'],
    'hungary': ['hungary', 'hungarian'],
    'romania': ['romania', 'romanian'],
    'bulgaria': ['bulgaria', 'bulgarian'],
    'serbia': ['serbia', 'serbian'],
    'croatia': ['croatia', 'croatian'],
    'slovenia': ['slovenia', 'slovenian'],
    'ukraine': ['ukraine', 'ukrainian'],
    'belarus': ['belarus', 'belarusian', 'byelorussia'],
    'estonia': ['estonia', 'estonian'],
    'latvia': ['latvia', 'latvian'],
    'lithuania': ['lithuania', 'lithuanian'],
    'pakistan': ['pakistan', 'pakistani'],
    'bangladesh': ['bangladesh', 'bangladeshi'],
    'nepal': ['nepal', 'nepalese'],
    'bhutan': ['bhutan', 'bhutanese'],
    'mongolia': ['mongolia', 'mongolian'],
    'kazakhstan': ['kazakhstan', 'kirghizstan', 'kyrgyzstan'],
    'uzbekistan': ['uzbekistan', 'uzbek'],
    'turkmenistan': ['turkmenistan', 'turkmen'],
    'tajikistan': ['tajikistan', 'tadjikistan', 'tajik'],
    'afghanistan': ['afghanistan', 'afghan'],
    'iraq': ['iraq', 'iraqi'],
    'syria': ['syria', 'syrian'],
    'lebanon': ['lebanon', 'lebanese'],
    'israel': ['israel', 'israeli'],
    'palestine': ['palestine', 'palestinian'],
    'jordan': ['jordan', 'jordanian'],
    'yemen': ['yemen', 'yemeni'],
    'oman': ['oman', 'omani'],
    'qatar': ['qatar', 'qatari'],
    'bahrain': ['bahrain', 'bahraini'],
    'kuwait': ['kuwait', 'kuwaiti'],
    'egypt': ['egypt', 'egyptian'],
    'libya': ['libya', 'libyan'],
    'sudan': ['sudan', 'sudanese'],
    'ethiopia': ['ethiopia', 'ethiopian'],
    'somalia': ['somalia', 'somali'],
    'uganda': ['uganda', 'ugandan'],
    'tanzania': ['tanzania', 'tanzanian'],
    'mozambique': ['mozambique', 'mozambican'],
    'zimbabwe': ['zimbabwe', 'zimbabwean'],
    'namibia': ['namibia', 'namibian'],
    'botswana': ['botswana', 'motswana'],
    'lesotho': ['lesotho', 'basotho'],
    'eswatini': ['eswatini', 'swaziland', 'swazi'],
    'malawi': ['malawi', 'malawian'],
    'zambia': ['zambia', 'zambian'],
    'senegal': ['senegal', 'senegalese'],
    'ghana': ['ghana', 'ghanaian'],
    'côte d\'ivoire': ['côte d\'ivoire', 'ivory coast', 'cote d\'ivoire'],
    'mali': ['mali', 'malian'],
    'mauritius': ['mauritius', 'mauritian'],
    'réunion': ['réunion', 'reunion'],
    'madagascar': ['madagascar', 'malagasy'],
    'new caledonia': ['new caledonia', 'caledonia'],
    'fiji': ['fiji', 'fijian'],
    'samoa': ['samoa', 'samoan'],
    'tonga': ['tonga', 'tongan'],
    'kiribati': ['kiribati'],
    'tuvalu': ['tuvalu'],
    'nauru': ['nauru', 'nauruan'],
    'palau': ['palau', 'palauan'],
    'micronesia': ['micronesia', 'micronesian'],
    'marshall islands': ['marshall islands', 'marshallese'],
    'vanuatu': ['vanuatu'],
    'solomon islands': ['solomon islands', 'solomon'],
    'french polynesia': ['french polynesia', 'polynesia'],
    'cook islands': ['cook islands'],
    'niue': ['niue'],
    'tokelau': ['tokelau'],
    'wallis and futuna': ['wallis and futuna', 'wallis', 'futuna'],
    'guam': ['guam', 'guamanian'],
    'northern mariana islands': ['northern mariana', 'saipan'],
    'american samoa': ['american samoa'],
    'virgin islands': ['virgin islands', 'u.s. virgin islands'],
    'puerto rico': ['puerto rico', 'puerterico'],
    'åland islands': ['åland', 'aland', 'åland islands'],
    'faroe islands': ['faroe', 'faroese', 'färöer'],
    'san marino': ['san marino'],
    'monaco': ['monaco', 'monégasque'],
    'liechtenstein': ['liechtenstein'],
    'andorra': ['andorra', 'andorran'],
    'luxembourg': ['luxembourg', 'luxembourgish'],
    'malta': ['malta', 'maltese'],
    'cyprus': ['cyprus', 'cypriot'],
    'mauritania': ['mauritania', 'mauritanian'],
    'djibouti': ['djibouti', 'djiboutian'],
    'comoros': ['comoros', 'comorian'],
    'seychelles': ['seychelles', 'seychellois'],
    'cape verde': ['cape verde', 'caboverdean'],
    'são tomé and príncipe': ['são tomé', 'sao tome', 'santo tomé'],
    'equatorial guinea': ['equatorial guinea', 'equatoguinean'],
    'gabon': ['gabon', 'gabonese'],
    'central african republic': ['central african', 'c.a.r.', 'car'],
    'congo': ['congo', 'congolese'],
    'democratic republic of congo': ['drc', 'democratic republic of congo', 'dr congo'],
    'benin': ['benin', 'beninese'],
    'togo': ['togo', 'togolese'],
    'mauritius': ['mauritius', 'mauritian'],
    'sierra leone': ['sierra leone', 'sierra leonean'],
    'liberia': ['liberia', 'liberian'],
    'guinea': ['guinea', 'guinean'],
    'guinea-bissau': ['guinea-bissau', 'bissau-guinean'],
    'gambia': ['gambia', 'gambian'],
    'burkina faso': ['burkina faso', 'burkinabe'],
    'niger': ['niger', 'nigerien'],
    'nigeria': ['nigeria', 'nigerian'],
    'kenya': ['kenya', 'kenyan'],
    'rwanda': ['rwanda', 'rwandan'],
    'burundi': ['burundi', 'burundian'],
}

# Reverse mapping: create quick lookup from alias to canonical country name
ALIAS_TO_COUNTRY = {}
for country, aliases in COUNTRIES_AND_ALIASES.items():
    for alias in aliases:
        ALIAS_TO_COUNTRY[alias.lower()] = country

# List of columns to check for country information
COLUMNS_TO_CHECK = ['biome', 'interaction_notes', 'plant_host', 'tissue', 'guild']

def find_country_in_text(text, exclude_common_words=True):
    """
    Search for country names in text.
    Returns: canonical country name if found, None otherwise.
    
    Args:
        text: String to search
        exclude_common_words: If True, skip single-letter matches and very common words
    
    Returns:
        Canonical country name or None
    """
    if not text or not isinstance(text, str):
        return None
    
    text_lower = text.lower().strip()
    
    # Skip very short strings
    if len(text_lower) < 2:
        return None
    
    # Direct lookup - check if entire text matches an alias
    if text_lower in ALIAS_TO_COUNTRY:
        return ALIAS_TO_COUNTRY[text_lower]
    
    # Substring matching - check if any alias appears as a standalone word
    # This helps catch "xishuangbanna" → not a country, but other regional terms
    for alias, country in ALIAS_TO_COUNTRY.items():
        # Use word boundaries to avoid partial matches like "in" matching "india"
        # Only check for country if it's a meaningful length
        if len(alias) > 2:
            pattern = r'\b' + re.escape(alias) + r'\b'
            if re.search(pattern, text_lower):
                return country
    
    return None


def consolidate_country_data(row, headers):
    """
    Check multiple columns for country information and consolidate into 'country' column.
    
    Args:
        row: List representing a CSV row
        headers: List of header names
    
    Returns:
        Modified row with consolidated country data
    """
    h_idx = {name: i for i, name in enumerate(headers)}
    
    # Get current country value
    current_country = None
    if 'country' in h_idx and row[h_idx['country']]:
        current_country = row[h_idx['country']].lower().strip()
    
    # If country already has a good value, don't override
    if current_country and current_country != 'na':
        return row
    
    # Check specified columns for country information
    for col_name in COLUMNS_TO_CHECK:
        if col_name in h_idx and row[h_idx[col_name]]:
            cell_value = row[h_idx[col_name]]
            found_country = find_country_in_text(cell_value)
            
            if found_country:
                # Found a country! Add it to country column
                if 'country' in h_idx:
                    existing = row[h_idx['country']].strip() if row[h_idx['country']] else ''
                    if existing and existing.lower() != 'na':
                        # Append if there's existing data
                        row[h_idx['country']] = existing + '; ' + found_country
                    else:
                        row[h_idx['country']] = found_country
                break  # Stop after first country found
    
    return row


def get_country_name(alias):
    """
    Convert any country alias to canonical name.
    Useful for standardizing country names in the country_iso_mapping in R.
    
    Args:
        alias: Country alias or name
    
    Returns:
        Canonical country name or original if not found
    """
    if not alias:
        return None
    return ALIAS_TO_COUNTRY.get(alias.lower(), None)


if __name__ == "__main__":
    # Test the utility
    test_cases = [
        'xishuangbanna',
        'china',
        'p.r. china',
        'western ghats',
        'united states of america',
        'somewhere in brazil',
        'unknown',
        'india ink'
    ]
    
    for test in test_cases:
        print(f"'{test}' -> {find_country_in_text(test)}")
