# InterHop

<span id='generated_code_start'></span>

## <i class='fa fa-file-alt' style='color: steelblue; margin-right: 5px;'></i> Projects

<details style = 'border: solid 1px #c0c0c0; padding: 5px 10px; margin: 5px 0;'>
<summary><span style = 'font-size:13px;'>LinkR demo</summary>
</details>

## <i class='fa fa-terminal' style='color: steelblue; margin-right: 5px;'></i> Plugins

<details style = 'border: solid 1px #c0c0c0; padding: 5px 10px; margin: 5px 0;'>
<summary><span style = 'font-size:13px;'>Console</summary>

NA</details>

<details style = 'border: solid 1px #c0c0c0; padding: 5px 10px; margin: 5px 0;'>
<summary><span style = 'font-size:13px;'>Datatable</summary>

NA</details>

<details style = 'border: solid 1px #c0c0c0; padding: 5px 10px; margin: 5px 0;'>
<summary><span style = 'font-size:13px;'>Development template</summary>
</details>

<details style = 'border: solid 1px #c0c0c0; padding: 5px 10px; margin: 5px 0;'>
<summary><span style = 'font-size:13px;'>Hospital stays</summary>

NA</details>

<details style = 'border: solid 1px #c0c0c0; padding: 5px 10px; margin: 5px 0;'>
<summary><span style = 'font-size:13px;'>Prescriptions</summary>

NA</details>

<details style = 'border: solid 1px #c0c0c0; padding: 5px 10px; margin: 5px 0;'>
<summary><span style = 'font-size:13px;'>Text reader</summary>
</details>

<details style = 'border: solid 1px #c0c0c0; padding: 5px 10px; margin: 5px 0;'>
<summary><span style = 'font-size:13px;'>Timeline {dygraphs}</summary>

# 1) Introduction

The "Timeline {dygraphs}" plugin allows you to display **continuous data** in the form of a **timeline**,
such as **vital signs** or **laboratory data**.

<div style="text-align: center;">
    <img 
        src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/img/plugins/timeline_dygraphs/figure.png"
        alt="Dygraphs timeline"
        style="width: 100%; max-width: 800px;"
    />
</div>

# 2) Features

## Figure settings

<div style="text-align: center;">
    <img 
        src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/img/plugins/timeline_dygraphs/en_figure_settings.png"
        alt="Figure settings"
        style="width: 100%; max-width: 250px; border: solid 1px #ccc; padding: 5px;"
    />
</div>

Here are the parameters you can adjust:

- **Data to display**: choose whether you want to display patient data or visit data
- **Concepts**: which concepts do you want to see on the figure?
This list includes the concepts you selected when creating the widget.
- **Synchronize timelines**: several plugins work with a timeline
(the scroll bar below the timeline where you can choose the period to display).
This feature allows you to synchronize the timelines of several widgets on the same tab.

## Code

<div style="text-align: center;">
    <img 
        src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/img/plugins/timeline_dygraphs/figure_and_code.png"
        alt="Figure and code"
        style="width: 100%; max-width: 800px; border: solid 1px #ccc; padding: 5px;"
    />
</div>

As with **all plugins** on LinkR, the **low-code** interface allows you to generate code from the graphical interface.

To do this, go to the "Figure settings" tab, and once you have chosen your parameters, click on "Display figure".

This will:

- **generate the code** corresponding to the chosen parameters
- then **execute the code** and display the figure corresponding to the code

You can then **modify the code** directly and save it.

If you click on "Run code" from the tab with the code editor, it will display the figure corresponding to the code.

However, if you are on the tab corresponding to the figure settings, it will erase the current code and replace it
with the code corresponding to the current parameters.

## Save files

<div style="text-align: center;">
    <img 
        src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/img/plugins/timeline_dygraphs/en_settings_files.png"
        alt="Settings files"
        style="width: 100%; max-width: 280px; border: solid 1px #ccc; padding: 5px;"
    />
</div>

You can **save** the figure parameters and code from the save files management page.

To do this, click on the name of the selected file at the top of the widget ("Save File 1" in the screenshot above).
You can then **add** or **delete** save files.

Then return to the "Figure settings" or "Code" tabs and save your parameters and code.

## General settings

<div style="text-align: center;">
    <img 
        src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/img/plugins/timeline_dygraphs/en_general_settings.png"
        alt="General settings"
        style="width: 100%; max-width: 280px; border: solid 1px #ccc; padding: 5px;"
    />
</div>

The general settings are divided into:

- **Display Settings**

    - **Show save file**: show or hide the name of the selected save file
    - **Figure and settings/editor side by side**: do you want the figure to appear side by side
    with the figure settings or the figure code? This avoids going back and forth between
    the different tabs during the widget configuration phase.

- **Code Execution**

    - **Run code when loading a save file**: this allows the code to be executed when
    the selected save file changes, which is useful when loading a project:
    this allows all widgets with this option to be loaded
    - **Run code when data is updated**: for this widget, this means that
    the data is updated when you change patient or visit, depending on the value of the 
    "Data to display" parameter</details>

## <i class='fa fa-database' style='color: steelblue; margin-right: 5px;'></i> Datasets

<details style = 'border: solid 1px #c0c0c0; padding: 5px 10px; margin: 5px 0;'>
<summary><span style = 'font-size:13px;'>MIMIC-IV demo set</summary>

The <a href="https://mimic.mit.edu/" target="_blank">MIMIC</a> database, or Medical Information Mart for Intensive Care, is a North American database containing data from over <strong>50,000 patients</strong> admitted to intensive care units. It is one of the most widely used critical care databases due to its free access.

Despite its imperfect data quality, it provides a solid foundation for <strong>learning to handle</strong> data from <strong>health data warehouses</strong> (HDW).

The database exists in several versions, with the most recent being MIMIC-IV.

### Test Data (Public Access)

The MIMIC database includes <strong>test datasets</strong> for versions III and IV, containing anonymized data from 100 patients, which are publicly accessible.

You can download the data here:

- <a href="https://physionet.org/content/mimiciii-demo/1.4/" target="_blank">MIMIC-III Test</a>: data with the <a href="https://mimic.mit.edu/docs/iii/tables/" target="_blank">MIMIC data schema</a>
- <a href="https://physionet.org/content/mimic-iv-demo-omop/0.9/" target="_blank">MIMIC-IV OMOP Test</a>: data with the <a href="https://ohdsi.github.io/CommonDataModel/cdm54.html" target="_blank">OMOP data schema</a>

### Full Data Access

To access the <strong>complete datasets</strong>, certain steps need to be completed.

Visit the <a href="https://physionet.org/content/mimiciii/1.4/" target="_blank">MIMIC-III database page</a>.

At the bottom of the page, you’ll see this alert:

<div style="background-color: #FBE1DE; padding: 10px; border-radius: 10px;" role="alert">
  This is a restricted-access resource. To access the files, you must fulfill all of the following requirements:
  <ul>
    <li>be a <a href="https://physionet.org/login/?next=/settings/credentialing/" target="_blank">credentialed user</a></li>
    <li>complete required training:</li>
        <ul>
            <li><a href="https://physionet.org/login/?next=/content/mimiciii/view-required-training/1.4/#1" target="_blank">CITI Data or Specimens Only Research</a></li>
            You may submit your training <a href="https://physionet.org/login/?next=/settings/training/" target="_blank">here</a>.
        </ul>
            <li>
            <a href="https://physionet.org/login/?next=/sign-dua/mimiciii/1.4/" target="_blank">sign the data use agreement</a> for the project
            </li>
  </ul>
</div>

You will need to start by registering on the <a href="https://physionet.org/register/" target="_blank">physionet.org</a> website.

Then, you must submit an <a href="https://physionet.org/settings/credentialing/" target="_blank">access request</a> to Physionet, providing some information and the contact details of a supervisor or colleague, who will receive an email.

Next, you must complete the CITI Course, a required training to access data hosted on Physionet. The steps are <a href="https://physionet.org/about/citi-course/" target="_blank">detailed here</a>.

Once the CITI Course is completed, you can <strong>download the certificate</strong> and <a href="https://physionet.org/settings/training/" target="_blank">upload it here</a> for validation by the Physionet team.

The final step is to sign the <a href="https://physionet.org/login/?next=/sign-dua/mimiciii/1.4/" target="_blank">data use agreement</a>.
</details>

## <i class='fa fa-code' style='color: steelblue; margin-right: 5px;'></i> Data cleaning scripts

No data cleaning scripts available.

<span id='generated_code_end'></span>
