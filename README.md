## InterHop

<i class="fa fa-check" style="color: steelblue;"></i> InterHop is a French **non-profit association**. It promotes, develops and provides **free and open-source software for health**.

<i class="fa fa-check" style="color: steelblue;"></i> InterHop unites **activists** advocating for **open source software** and the **self-managed utilization** of health data at the **local level**.

<i class="fa fa-check" style="color: steelblue;"></i> We are a collective comprised of **engineers, lawyers, health professionals** and **patients**.

<i class="fa fa-check" style="color: steelblue;"></i> Here's the **open-source content** we share for the **LinkR** application.

## <i class='fa fa-file-alt' style='color: steelblue;'></i> Studies

No studies available.

## <i class='fa fa-terminal' style='color: steelblue;'></i> Plugins - Patient-level data

<details style = 'border:solid 1px; padding:10px;'>
<summary><span style = 'font-size:15px;'>R console</summary>

### <i class="fa fa-info-circle" style="color: steelblue;"></i> 1) Description

This plugin allows you to **execute code** in the R console and **save your code as scripts**.

### <i class="fa fa-cogs" style="color: steelblue;"></i> 2) Usage

#### <i class="fa fa-cogs" style="color: steelblue;"></i> a) Scripts management

Go to the "Scripts management" tab to **add, delete, or rename scripts**.

A script is a **text file containing code**.

Once a script is created, you can **edit** it in the "Script" tab and **execute the code**.

#### <i class="fa fa-code" style="color: steelblue;"></i> b) R code

Write **R code** and execute it: the **result returned by the console** will appear below the text editor.

To know the **data model** used by the application, click on the help button (question mark at the top right of the page) when you are on the "Data / Access Data" page.

Example code (get the min, max, and average heart rate of our patients):

```
d$measurement %>%
    dplyr::filter(measurement_concept_id == 3027018) %>%
    dplyr::group_by(person_id) %>%
    dplyr::summarize(min_weight = min(value_as_number), max_weight = max(value_as_number), avg_weight = mean(value_as_number)) %>%
    dplyr::ungroup()
```

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/patient_lvl/3c64dcfb08b95020e4b06ee78c1ca48158fed7657cf01621e92de2a4be77bf68/r_code_1.png" width="900" style="border:dashed 1px; padding:10px;"/>

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/patient_lvl/3c64dcfb08b95020e4b06ee78c1ca48158fed7657cf01621e92de2a4be77bf68/r_code_2.png" width="600" style="margin-left:-7px;"/>

#### <i class="fa fa-file-code-o" style="color: steelblue;"></i> c) RMarkdown

You can also write code in **Rmarkdown**.

This is **Markdown** to which you can add **R code**.

For more information on RMarkdown, <a href="https://rmarkdown.rstudio.com/lesson-2.html" target="_blank">visit their site</a>.

Example code (the same example as above, in RMarkdown - a backslash has been added to prevent code execution):

```
# Script on heart rate

\```{r}
d$measurement %>%
dplyr::filter(measurement_concept_id == 3027018) %>%
dplyr::group_by(person_id) %>%
dplyr::summarize(min_weight = min(value_as_number), max_weight = max(value_as_number), avg_weight = mean(value_as_number)) %>%
dplyr::ungroup()
\```
```

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/patient_lvl/3c64dcfb08b95020e4b06ee78c1ca48158fed7657cf01621e92de2a4be77bf68/rmarkdown_1.png" width="900" style="border:dashed 1px; padding:10px;" />

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/patient_lvl/3c64dcfb08b95020e4b06ee78c1ca48158fed7657cf01621e92de2a4be77bf68/rmarkdown_2.png" width="700" style="border:dashed 1px; padding:10px;" />

#### <i class="fa fa-bar-chart" style="color: steelblue;"></i> d) Figure

You can **create figures**, for example with the `ggplot2` library.

Here's an example of code:

```
# A list containing the data for the plot
data <- list()

# Filter data
data$x <- d$measurement %>% dplyr::filter(measurement_concept_id == 3027018)

# Create ggplot2 plot
data$x %>%
    ggplot2::ggplot(ggplot2::aes(x = value_as_number)) +
    ggplot2::geom_histogram(bins = 50, fill = "#377EB8", color = "#FFFFFF") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Heart rate (bpm)", y = "")
```

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/patient_lvl/3c64dcfb08b95020e4b06ee78c1ca48158fed7657cf01621e92de2a4be77bf68/r_plot_1.png" width="600" style="border:dashed 1px; padding:10px;" />

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/patient_lvl/3c64dcfb08b95020e4b06ee78c1ca48158fed7657cf01621e92de2a4be77bf68/r_plot_2.png" width="600" style="border:dashed 1px; padding:10px;" />
</details>

<details style = 'border:solid 1px; padding:10px;'>
<summary><span style = 'font-size:15px;'>Notes</summary>


</details>




## <i class='fa fa-terminal' style='color: steelblue;'></i> Plugins - Aggregated data

<details style = 'border:solid 1px; padding:10px;'>
<summary><span style = 'font-size:15px;'>R console</summary>

### <i class="fa fa-info-circle" style="color: steelblue;"></i> 1) Description

This plugin allows you to **execute code** in the R console and **save your code as scripts**.

### <i class="fa fa-cogs" style="color: steelblue;"></i> 2) Usage

#### <i class="fa fa-cogs" style="color: steelblue;"></i> a) Scripts management

Go to the "Scripts management" tab to **add, delete, or rename scripts**.

A script is a **text file containing code**.

Once a script is created, you can **edit** it in the "Script" tab and **execute the code**.

#### <i class="fa fa-code" style="color: steelblue;"></i> b) R code

Write **R code** and execute it: the **result returned by the console** will appear below the text editor.

To know the **data model** used by the application, click on the help button (question mark at the top right of the page) when you are on the "Data / Access Data" page.

Example code (get the min, max, and average heart rate of our patients):

```
d$measurement %>%
    dplyr::filter(measurement_concept_id == 3027018) %>%
    dplyr::group_by(person_id) %>%
    dplyr::summarize(min_weight = min(value_as_number), max_weight = max(value_as_number), avg_weight = mean(value_as_number)) %>%
    dplyr::ungroup()
```

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/aggregated/6f3c30ede116bc25978075b6634268214c545173634f3cd81c0d1db6081a45b8/r_code_1.png" width="900" style="border:dashed 1px; padding:10px;"/>

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/aggregated/6f3c30ede116bc25978075b6634268214c545173634f3cd81c0d1db6081a45b8/r_code_2.png" width="600" style="margin-left:-7px;"/>

#### <i class="fa fa-file-code-o" style="color: steelblue;"></i> c) RMarkdown

You can also write code in **Rmarkdown**.

This is **Markdown** to which you can add **R code**.

For more information on RMarkdown, <a href="https://rmarkdown.rstudio.com/lesson-2.html" target="_blank">visit their site</a>.

Example code (the same example as above, in RMarkdown - a backslash has been added to prevent code execution):

```
# Script on heart rate

\```{r}
d$measurement %>%
dplyr::filter(measurement_concept_id == 3027018) %>%
dplyr::group_by(person_id) %>%
dplyr::summarize(min_weight = min(value_as_number), max_weight = max(value_as_number), avg_weight = mean(value_as_number)) %>%
dplyr::ungroup()
\```
```

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/aggregated/6f3c30ede116bc25978075b6634268214c545173634f3cd81c0d1db6081a45b8/rmarkdown_1.png" width="900" style="border:dashed 1px; padding:10px;" />

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/aggregated/6f3c30ede116bc25978075b6634268214c545173634f3cd81c0d1db6081a45b8/rmarkdown_2.png" width="700" style="border:dashed 1px; padding:10px;" />

#### <i class="fa fa-bar-chart" style="color: steelblue;"></i> d) Figure

You can **create figures**, for example with the `ggplot2` library.

Here's an example of code:

```
# A list containing the data for the plot
data <- list()

# Filter data
data$x <- d$measurement %>% dplyr::filter(measurement_concept_id == 3027018)

# Create ggplot2 plot
data$x %>%
    ggplot2::ggplot(ggplot2::aes(x = value_as_number)) +
    ggplot2::geom_histogram(bins = 50, fill = "#377EB8", color = "#FFFFFF") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Heart rate (bpm)", y = "")
```

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/aggregated/6f3c30ede116bc25978075b6634268214c545173634f3cd81c0d1db6081a45b8/r_plot_1.png" width="600" style="border:dashed 1px; padding:10px;" />

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/aggregated/6f3c30ede116bc25978075b6634268214c545173634f3cd81c0d1db6081a45b8/r_plot_2.png" width="600" style="border:dashed 1px; padding:10px;" />
</details>

<details style = 'border:solid 1px; padding:10px;'>
<summary><span style = 'font-size:15px;'>Plot (ggplot2)</summary>

### <i class="fa fa-info-circle" style="color: steelblue;"></i> 1) Description

This plugin uses the R library <a href="https://ggplot2.tidyverse.org/" target="_blank">`ggplot2`</a>, allowing the creation of plots from data.

### <i class="fa fa-cogs" style="color: steelblue;"></i> 2) Usage

<details>
<summary><span style="--hover-color:#129AFD;cursor:pointer;text-decoration-line:underline;" onmouseover="this.style.color=this.style.getPropertyValue('--hover-color')" onmouseout="this.style.color=''">
Click here to show / hide content</span></summary>

#### <i class="fa fa-check" style="color: steelblue;"></i> a) Manage Scripts

In order to **save multiple plots** to a single widget, you can **create scripts** from the "Scripts management" tab.

You can also **rename** and **delete** scripts from this tab.

A script includes:

- The **parameters** saved on a plot, from the "Plot" tab.
- The **code** corresponding to that plot, from the "Code" tab.

Once a **script is created**, **select it** from the "Plot" or "Code" tab.

**Save** this script after making changes.

<br /><img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/aggregated/dbddcb34874bd3e8cfcc287f4dc73f422007d928e591add209329f03c99efa21/scripts_management.png" width="850" style="border:dashed 1px; padding:10px;" />

#### <i class="fa fa-check" style="color: steelblue;"></i> b) Configure a Plot

Go to the "Plot" tab.

**Choose the variable** to display on each axis in the "Variables" tab.

Some plots will have a **variable on only one axis** (geom_histogram), while others will only display **if variables are specified on both axes** (geom_point).

For each plot, choose:

- The **theme** of the plot.
- The **text** for the **x** axis.
- The **text** for the **y** axis (whether a variable is assigned to it or not).

In the "Variables" tab, it's possible to **group data**, by patient or by time. Then choose the **function** to apply to group the data.

For example:

- Group data by **patient** by selecting the **"maximum"** function: the maximum value of the parameter, across all stays, will be chosen for each patient.
- Group data every **4 hours** by selecting the **"average"** function: the maximum value will be chosen for each 4-hour interval, for each patient (thus, multiple data points per patient).

By checking "Show plot at script launch", the **plot** will be **displayed upon the script's loading**.

<br /><img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/aggregated/dbddcb34874bd3e8cfcc287f4dc73f422007d928e591add209329f03c99efa21/plot_settings.png" width="950" style="border:dashed 1px; padding:10px;" />

#### <i class="fa fa-check" style="color: steelblue;"></i> c) Display the Code

By clicking on "**Generate Code**" from the "Plot" tab, the **code corresponding to the plot** will be **generated** in the text editor, in the "Code" tab.

You can **edit this code** and **save** your changes.

If you click on **Generate Code** again, your code will be deleted and replaced by the default code corresponding to the plot configured in the "Plot" tab.

Click on "**Run code**" to display the plot corresponding to the code.

By checking "Run code at script launch", the **code** will be **executed upon the script's loading**.

<br /><img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/aggregated/dbddcb34874bd3e8cfcc287f4dc73f422007d928e591add209329f03c99efa21/plot_code.png" width="750" style="border:dashed 1px; padding:10px;" />

</details>

### <i class="fa fa-bar-chart" style="color: steelblue;"></i> 3) Available Plots

<details open>
<summary><span style="--hover-color:#129AFD;cursor:pointer;text-decoration-line:underline;" onmouseover="this.style.color=this.style.getPropertyValue('--hover-color')" onmouseout="this.style.color=''">
Click here to show / hide content</span></summary>

#### <i class="fa fa-check" style="color: steelblue;"></i> a) Histogram (geom_histogram)

Allows visualization of the distribution of a single continuous variable by dividing the x-axis into intervals and counting the number of observations in each interval.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/aggregated/dbddcb34874bd3e8cfcc287f4dc73f422007d928e591add209329f03c99efa21/geom_histogram.png" width="700" />

You can choose the **size of the bars** in two ways:

- Either by the **size** of the bars, depending on the x-axis (a size of 50 means a bar will take up 50 of the unit specified on the x-axis).
- Or by the **number** of total bars displayed.

#### <i class="fa fa-check" style="color: steelblue;"></i> b) Scatter Plot (geom_point)

Allows visualization of the distribution of two continuous variables, one on the x-axis and the other on the y-axis.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins/aggregated/dbddcb34874bd3e8cfcc287f4dc73f422007d928e591add209329f03c99efa21/geom_point.png" width="700" />

By default, only points having a **value at a given moment** both for the **x variable** and for the **y variable** will be displayed.

For instance, if I choose systolic blood pressure for x and mean arterial pressure for y (as in the example), and I have an x value on a certain day at 18:37:10 and a y value on the same day at 18:37:11, since the moments aren't exactly the same, there won't be a corresponding point on the graph.

To counter this, it is possible to **group data**, by patient or by time (see Usage section).

</details>
</details>




## <i class='fa fa-code' style='color: steelblue;'></i> Scripts

<details style = 'border:solid 1px; padding:10px;'>
<summary><span style = 'font-size:15px;'>abc</summary>


</details>




## <i class='fa fa-database' style='color: steelblue;'></i> Datasets

<details style = 'border:solid 1px; padding:10px;'>
<summary><span style = 'font-size:15px;'>MIMIC-IV demo</summary>


</details>

<details style = 'border:solid 1px; padding:10px;'>
<summary><span style = 'font-size:15px;'>MIMIC-III</summary>


</details>




## <i class='fa fa-list' style='color: steelblue;'></i> Vocabularies

<details style = 'border:solid 1px; padding:10px;'>
<summary><span style = 'font-size:15px;'>MIMIC-III</summary>


</details>

<details style = 'border:solid 1px; padding:10px;'>
<summary><span style = 'font-size:15px;'>MIMIC-IV demo</summary>


</details>



