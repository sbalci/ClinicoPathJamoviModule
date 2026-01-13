import glob
import os
import re

# Paths
jamovi_path = 'jamovi'
r_path = 'R'
output_markdown = 'vignettes/function_list.md'

files = glob.glob(os.path.join(jamovi_path, '*.a.yaml'))
functions = []

mapping = {
    'Exploration': 'ClinicoPathDescriptives',
    'Survival': 'jsurvival',
    'meddecide': 'meddecide',
    'JJStatsPlot': 'jjstatsplot',
    'OncoPath': 'OncoPath'
}

def get_implementation_status(func_name):
    r_file = os.path.join(r_path, f"{func_name}.b.R")
    if not os.path.exists(r_file):
        return "No R File"
    
    size = os.path.getsize(r_file)
    if size < 600: # Template files are usually < 500 bytes
        return "Template Only"
    return "Coded"

for f in files:
    try:
        with open(f, 'r') as file:
            content = file.read()
            
        name_match = re.search(r'^name:\s*(.+)$', content, re.MULTILINE)
        title_match = re.search(r'^title:\s*(.+)$', content, re.MULTILINE)
        menu_match = re.search(r'^menuGroup:\s*(.+)$', content, re.MULTILINE)
        subtitle_match = re.search(r'^menuSubtitle:\s*(.+)$', content, re.MULTILINE)
        
        if not name_match:
            continue
            
        name = name_match.group(1).strip().strip("'").strip('"')
        title = title_match.group(1).strip().strip("'").strip('"') if title_match else ""
        subtitle = subtitle_match.group(1).strip().strip("'").strip('"') if subtitle_match else ""
        
        menu_group = 'Uncategorized'
        if menu_match:
            menu_group = menu_match.group(1).strip().strip("'").strip('"')
        
        group_status = 'Stable'
        clean_group = menu_group
        
        if menu_group.endswith('D'):
            group_status = 'Draft'
            clean_group = menu_group[:-1]
        elif menu_group.endswith('T'):
            group_status = 'Test'
            clean_group = menu_group[:-1]
            
        category = mapping.get(clean_group, clean_group)
        impl_status = get_implementation_status(name)
        
        functions.append({
            'name': name,
            'title': title,
            'subtitle': subtitle,
            'group_status': group_status,
            'category': category,
            'original_group': menu_group,
            'implementation': impl_status
        })
            
    except Exception as e:
        print(f"Error parsing {f}: {e}")

# Sort by Category, then Group Status (Stable, Test, Draft), then Implementation (Coded, Template, No File), then Name
status_order = {'Stable': 0, 'Test': 1, 'Draft': 2}
impl_order = {'Coded': 0, 'Template Only': 1, 'No R File': 2}

functions.sort(key=lambda x: (
    x['category'], 
    status_order.get(x['group_status'], 3), 
    impl_order.get(x['implementation'], 3),
    x['name']
))

# Generate Markdown
md = "# ClinicoPath Jamovi Module Function List\n\n"
md += "This list is automatically generated. Functions marked as **Draft** or **Template Only** are still in development.\n\n"

current_cat = None
current_status = None

for func in functions:
    if func['category'] != current_cat:
        current_cat = func['category']
        md += f"\n## {current_cat}\n"
        current_status = None # Reset status for new category
    
    if func['group_status'] != current_status:
        current_status = func['group_status']
        md += f"\n### {current_status} Status\n\n"
        md += "| Function Name | Title | Implementation | Original Group |\n"
        md += "|---|---|---|---|\n"
        
    impl_display = f"**{func['implementation']}**" if func['implementation'] == 'Coded' else func['implementation']
    
    md += f"| `{func['name']}` | {func['title']} | {impl_display} | {func['original_group']} |\n"

with open(output_markdown, 'w') as f:
    f.write(md)

print(f"{output_markdown} updated systematically.")

