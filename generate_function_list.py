import glob
import os
import re

files = glob.glob('jamovi/*.a.yaml')
functions = []

mapping = {
    'Exploration': 'ClinicoPathDescriptives',
    'Survival': 'jsurvival',
    'meddecide': 'meddecide',
    'JJStatsPlot': 'jjstatsplot',
    'OncoPath': 'OncoPath'
}

for f in files:
    try:
        with open(f, 'r') as file:
            content = file.read()
            
        name_match = re.search(r'^name:\s*(.+)$', content, re.MULTILINE)
        menu_match = re.search(r'^menuGroup:\s*(.+)$', content, re.MULTILINE)
        
        if not name_match:
            continue
            
        name = name_match.group(1).strip()
        # Some names might be quoted
        name = name.strip("'").strip('"')
        
        menu_group = 'Uncategorized'
        if menu_match:
            menu_group = menu_match.group(1).strip()
            menu_group = menu_group.strip("'").strip('"')
        
        status = 'Stable'
        clean_group = menu_group
        
        if menu_group.endswith('D'):
            status = 'Draft'
            clean_group = menu_group[:-1]
        elif menu_group.endswith('T'):
            status = 'Test'
            clean_group = menu_group[:-1]
            
        category = mapping.get(clean_group, clean_group)
        
        functions.append({
            'name': name,
            'status': status,
            'category': category,
            'original_group': menu_group
        })
            
    except Exception as e:
        print(f"Error parsing {f}: {e}")

# Sort by Category, then Status (Stable, Test, Draft), then Name
status_order = {'Stable': 0, 'Test': 1, 'Draft': 2}
functions.sort(key=lambda x: (
    x['category'], 
    status_order.get(x['status'], 3), 
    x['name']
))

# Generate Markdown
md = "# ClinicoPath Jamovi Module Function List\n\n"
current_cat = None
current_status = None

for func in functions:
    if func['category'] != current_cat:
        current_cat = func['category']
        md += f"\n## {current_cat}\n"
        current_status = None # Reset status for new category
    
    if func['status'] != current_status:
        current_status = func['status']
        md += f"\n### {current_status}\n\n| Function Name | Original Group |\n|---|---|\n"
        
    md += f"| `{func['name']}` | {func['original_group']} |\n"

output_path = 'vignettes/function_list.md'
with open(output_path, 'w') as f:
    f.write(md)

print(f"{output_path} created")
