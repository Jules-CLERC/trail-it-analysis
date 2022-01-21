# Trail-it Dashboard Documentation

![v2_Conceptual_model_Trail-it drawio-2](https://user-images.githubusercontent.com/48790000/150534480-2ed0c363-5c17-4a80-b014-dab301b3c472.png)

# Modules

the dashboard is divided into modules :

- Modules Data returns a data.
- Modules Page contains the skeleton of a page.
- Module Plot create the graph.

## Data

### Data_import

#### Load

This module takes care of the connection to the **MySQL** database in order to recover all the data which will then be sent to the **Server.R**.
The credentials are retrieved from the **credentials.csv** file.

```
mydb = dbConnect(MySQL(),
                     user=credentials[1, "username"],
                     password=credentials[1, "password"],
                     dbname=credentials[1, "dbname"],
                     host=credentials[1, "host"])
```

#### Preprocess

in order to resolve problems, bugs and conflicts, modifications are made to our data :

- Add a column **Timestamp**
- Delete duplicate rows
- Remove rows that have a negative reaction time
- Add a column **sessionID** 

### Data_list_players

this module creates from **D**, a new **data.frame** which gathers the **profileID** and their **playerName**. The two columns are concatenated in the column **playerNameID**. As in the example below :

|profileID                |playerName                          |playerNameID                         |
|----------------|-------------------------------|-----------------------------|
|id0|name1          |name1 ( id0 )           |
|id1          |name2           |name2 ( id1 )

> **playerNameID** will be useful when the user will have to select a player. If two players have the same **playerName**, the user will be able to identify them by the **profileID** available in parentheses.        

### Data_select_player

This module takes care of changing the current player. It returns the **profileID**.

## Page

### page_individual_profile_information

<img width="1345" alt="Screenshot 2022-01-21 at 14 25 57" src="https://user-images.githubusercontent.com/48790000/150534703-f4b08732-42f9-4e04-9f6c-f847fe03d0dd.png">

### page_individual_performance_session

<img width="1345" alt="Screenshot 2022-01-21 at 14 26 55" src="https://user-images.githubusercontent.com/48790000/150534932-49bac3bd-ee09-49be-a1e0-e8e3a7a94921.png">
<img width="1345" alt="Screenshot 2022-01-21 at 14 27 09" src="https://user-images.githubusercontent.com/48790000/150534936-2e25bb9e-5684-4a39-ad00-d7c82fea5798.png">
<img width="1345" alt="Screenshot 2022-01-21 at 14 27 20" src="https://user-images.githubusercontent.com/48790000/150534940-c3f02aef-3d98-4f39-87f8-d122c895b54e.png">

### page_trends_statistics_players

<img width="1345" alt="Screenshot 2022-01-21 at 14 28 32" src="https://user-images.githubusercontent.com/48790000/150535166-b9aae47f-de40-4a1c-af83-80ab392855be.png">
<img width="1345" alt="Screenshot 2022-01-21 at 14 28 44" src="https://user-images.githubusercontent.com/48790000/150535175-ee2a70e8-0a34-45ae-bd1e-f2ab5be2f725.png">
<img width="1345" alt="Screenshot 2022-01-21 at 14 28 54" src="https://user-images.githubusercontent.com/48790000/150535177-1af62fbb-0557-442a-9b50-7076f604c6d5.png">

### page_trends_performance_players


<img width="1345" alt="Screenshot 2022-01-21 at 14 30 30" src="https://user-images.githubusercontent.com/48790000/150535490-8ef38862-5810-49ba-94b3-228861f06933.png">
<img width="1345" alt="Screenshot 2022-01-21 at 14 30 43" src="https://user-images.githubusercontent.com/48790000/150535519-3583ea2c-ec1b-40c5-abdc-e53f3034c6d4.png">
<img width="1345" alt="Screenshot 2022-01-21 at 14 30 48" src="https://user-images.githubusercontent.com/48790000/150535525-42b7beda-3ef2-4d66-ab8d-568e03812ae0.png">






