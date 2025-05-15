import requests
from bs4 import BeautifulSoup
import csv

# URL of the website with the table and pagination
base_url = "https://www.boxofficemojo.com/chart/top_lifetime_gross/?area=XWW"

# Number of pages to scrape
num_pages = 500  # Change this to the desired number of pages

# Initialize an empty list to store the scraped data
table_data = []

# Define custom headers
headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36",
    "Accept-Language": "en-US,en;q=0.5",
    # Add more headers if needed
}

# Loop through each page
for page in range(1, num_pages + 1):
    if page==1:
        url = f"{base_url}"      
    else:
        pageNr = (page*100)
        s = f"&offset={pageNr}"
        url = f"{base_url}{s}"
    
    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()  # Raise an exception for non-200 status codes
        
        soup = BeautifulSoup(response.text, 'html.parser')
        table = soup.find('table')
        
        if table:
            for row in table.find_all('tr'):
                row_data = [cell.text.strip() for cell in row.find_all('td')]
                if row_data:
                    table_data.append(row_data)
        else:
            break 
            #print(f"No table found on page {page}")
    except requests.exceptions.HTTPError as e:
        if e.response.status_code == 403:
            print(f"403 Forbidden error on page {page}. You may need to handle authorization or use proxies.")
        else:
            print(f"Failed to retrieve page {page} with status code {e.response.status_code}")
    except Exception as e:
        print(f"An error occurred: {str(e)}")

# Define the CSV file path
now = datetime.now()
csv_file = f"LifetimeGross{now}.csv"

# Write the scraped data to a CSV file
with open(csv_file, 'w', newline='') as csvfile:
    csv_writer = csv.writer(csvfile)
    for row in table_data:
        csv_writer.writerow(row)

print(f"Data scraped from {num_pages} pages and saved to {csv_file}")
