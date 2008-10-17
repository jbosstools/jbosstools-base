<%@ taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@ taglib uri="http://java.sun.com/jsf/html" prefix="h" %>

<html>
    <head>
    	<title>Input User Name Page</title>	
	   	 <link href="stylesheet/style15.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style14.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style13.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style12.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style11.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style10.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style9.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style8.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style7.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style6.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style5.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style4.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style3.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style2.css" rel="stylesheet" type="text/css" />
	   	 <link href="stylesheet/style1.css" rel="stylesheet" type="text/css" /> 	 
    	 <style>
    	 	.style-class9 {
				/* in style comment 1 */
				color: #0D5AAA;
				border: 1px solid #EEE000;
				/* in style comment 2 */
				font-size: 24px;
				background: #F0A8FF;
				/* in style comment 3 */
			}
			.style-class10 {
				/* in style comment 1 */
				color: #0D5AAA;
				border: 1px solid #EEE000;
				/* in style comment 2 */
				font-size: 24px;
				background: #F0A8FF;
				/* in style comment 3 */
			}
			</style>
    </head>
    <body>
	<f:view>
		<h1>
			<h:outputText value="Title1" styleClass="style-class1"/>
		</h1>
		<h1>
			<h:outputText value="Title2" styleClass="style-class2"/>
		</h1>
		<h1>
			<h:outputText value="Title3" styleClass="style-class3"/>
		</h1>
		<h1>
			<h:outputText value="Title4" styleClass="style-class4"/>
		</h1>
		<h:dataTable>
			<h:column><h:outputText value="Title5" styleClass="style-class5"/></h:column>
			<h:column><h:outputText value="Title6" styleClass="style-class6"/></h:column>
			<h:column><h:outputText value="Title7" styleClass="style-class7"/></h:column>
			<h:column><h:outputText value="Title8" styleClass="style-class8"/></h:column>
			<h:column><h:outputText value="Title9" styleClass="style-class9"/></h:column>
			<h:column><h:outputText value="Title10" styleClass="style-class10"/></h:column>
		</h:dataTable>
	</f:view>
    </body>
</html>