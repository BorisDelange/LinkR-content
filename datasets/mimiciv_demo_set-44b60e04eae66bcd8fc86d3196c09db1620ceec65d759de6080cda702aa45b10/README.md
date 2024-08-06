The [MIMIC](https://mimic.mit.edu/) database, which stands for Medical Information Mart for Intensive Care, is a North American database containing data from over **50,000 patients** admitted to intensive care. It is one of the most widely used intensive care databases, due to its free access.

Despite having data of imperfect quality, it serves as a good foundation for **learning to manipulate** data from **health data warehouses** (HDWs).

It exists in several versions, with the most recent being MIMIC-IV.

<br />
<div style="border-radius: 10px; background-color: #E5F4FB; padding: 10px; margin: 0;">
  <div style="display: flex; align-items: center;">
    <i class="fas fa-question-circle" style="color: #4283c4; margin-right: 10px;"></i>
    <p style="margin: 0;">Do you need programming knowledge?</p>
  </div>
</div>
<br />

No, it is **not necessarily required** to have programming knowledge to manipulate the data in this database.

Handling the database itself will require programming knowledge, especially in SQL. It is a database with a particular schema, which is difficult to grasp.

However, it is possible to manipulate this data entirely with a **graphical interface**: this is one of the reasons why **LinkR** was created.

<br />
<div style="border-radius: 10px; background-color: #E5F4FB; padding: 10px; margin: 0;">
  <div style="display: flex; align-items: center;">
    <i class="fas fa-question-circle" style="color: #4283c4; margin-right: 10px;"></i>
    <p style="margin: 0;">How to access the data?</p>
  </div>
</div>
<br />

The MIMIC database includes **test databases** for versions III and IV, which contain anonymized data from 100 patients and are publicly accessible.

You can download the data here:

- [MIMIC-III test](https://physionet.org/content/mimiciii-demo/1.4/): data with the [MIMIC](https://mimic.mit.edu/docs/iii/tables/) data schema
- [MIMIC-IV OMOP test](https://physionet.org/content/mimic-iv-demo-omop/0.9/): data with the [OMOP](https://ohdsi.github.io/CommonDataModel/cdm54.html) data schema

To access the **complete databases**, you need to complete a few steps.

Visit the [MIMIC-III database page](https://physionet.org/content/mimiciii/1.4/).

You will see this box at the bottom of the page:

<div class="alert alert-danger col-md-8" role="alert">
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

You must start by registering on the site [physionet.org](https://physionet.org/register/).

You will need to submit an [access request](https://physionet.org/settings/credentialing/) to Physionet, providing some information and the contact details of a supervisor or colleague, who will receive an email.

You will then need to complete the CITI Course, a necessary training to access data hosted on the Physionet site. The different steps are [detailed here](https://physionet.org/about/citi-course/).

You can then **download the certificate** once the CITI Course is completed, and you can [submit it here](https://physionet.org/settings/training/) for validation by the Physionet team.

Finally, you will need to sign the [data use agreement](https://physionet.org/login/?next=/sign-dua/mimiciii/1.4/).

