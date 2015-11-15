# MVC

* **Model** - *What* your application is (but not *how* it is displayed).
* **Controller** - *How* your Model is presented to the user (UI logic).
* **View** - Your Controller's minions.

It's all about managing communication between camps.

* Controllers can always talk directly to their Model.
* Controllers can also talk directly to their View (outlet).
* The Model and View should never never speak to each other.
* The Controller can drop a target on itself, then hand out an action to the View. The View sends the action when things happen in the UI.
* Sometimes the View needs to synchronize with the Controller. The Controller sets itself as the View's *delegate*. The delegate is set via a protocol.
* Views do not own the data they display.
* If needed, View has a protocol to acquire information about Model. Controllers are almost always that data source (not Model). 
* Model cannot talk to Controllers. The Model is UI independent.
* The Model uses a *radio station*-like broadcast mechanism to propogate information to others.
* Controllers or other Models *tune in* to interesting stuff.

```
 ______    _______       __        ___     _    ________    _______
/  ____\  /__   __\     /  \      |   \   | |  | _______|  / _____ \
| |____      | |       / /\ \     | |\ \  | |  | |_____   | /     \ |
\____  \     | |      / ____ \    | | \ \ | |  |  _____|  | |     | |
 ____| |     | |     / /    \ \   | |  \ \| |  | |        | \_____/ |
\______/	 |_|    /_/      \_\  |_|   \___|  |_|         \_______/
```