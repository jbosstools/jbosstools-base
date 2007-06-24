<%@page contentType="text/html;charset=UTF-8"%>
<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>
<%@ taglib prefix="s" uri="http://struts.apache.org/shale/core" %>

<%--

 Copyright 2004-2005 The Apache Software Foundation.  
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at
 
      http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

--%>

<f:view>
<%@include              file="messages.jspf"%>
<html>
<head>
<title>
  <h:outputText        value="#{messages['usecases.title']}"/>
</title>
</head>
<body>

 <h:form                   id="usecasesForm">

  <h1><h:outputText value="#{messages['usecases.primary']}"/></h1>

  <h:panelGrid        columns="1">

    <f:facet             name="header">
      <h:panelGroup>
        <h:outputText   value="#{messages['prompt.username']}"/>
        <h:outputText   value="#{user.username}"
                     rendered="#{!(empty user)}"/>
        <h:outputText   value="----"
                     rendered="#{empty user}"/>
      </h:panelGroup>
    </f:facet>

    <h:commandLink         id="jndi"
                       action="jndi$test">
      <h:outputText     value="#{messages['usecases.jndi']}"/>
    </h:commandLink>

    <h:commandLink         id="locale"
                       action="locale$select">
      <h:outputText     value="#{messages['usecases.locale']}"/>
    </h:commandLink>

<%--  
    <h:commandLink         id="logon"
                       action="#{logon$dialog.enter}"
                     rendered="#{empty user}">
      <h:outputText     value="#{messages['usecases.logon']}"/>
    </h:commandLink>

    <h:commandLink         id="edit"
                       action="#{logon$dialog.edit}"
                     rendered="#{!(empty user)}">
      <h:outputText     value="#{messages['usecases.edit']}"/>
    </h:commandLink>

    <h:commandLink         id="logoff"
                       action="#{logon$dialog.logoff}"
                     rendered="#{!(empty user)}">
      <h:outputText     value="#{messages['usecases.logoff']}"/>
    </h:commandLink>
--%>

    <h:commandLink         id="logon"
                       action="dialog:Log On"
                     rendered="#{empty user}">
      <h:outputText     value="#{messages['usecases.logon']}"/>
    </h:commandLink>

    <h:commandLink         id="edit"
                       action="dialog:Edit Profile"
                     rendered="#{!(empty user)}">
      <h:outputText     value="#{messages['usecases.edit']}"/>
    </h:commandLink>

    <h:commandLink         id="logoff"
                       action="#{profile$logon.logoff}"
                     rendered="#{!(empty user)}">
      <h:outputText     value="#{messages['usecases.logoff']}"/>
    </h:commandLink>

    <h:commandLink         id="subview"
                       action="subview$first">
      <h:outputText     value="#{messages['usecases.subview']}"/>
    </h:commandLink>

    <f:facet             name="footer">
      <h:panelGroup>
        <h:outputText   value="#{messages['prompt.locale']}"/>
        <h:outputText   value="#{view.locale}"/>
      </h:panelGroup>
    </f:facet>

  </h:panelGrid>

  <h1><h:outputText     value="#{messages['usecases.java']}"/></h1>

  <h:panelGrid        columns="1">

    <h:outputLink          id="javaCategories"
                        value="list/supportedCategories.remote"
                       target="_new">
      <h:outputText     value="#{messages['usecases.categories']}"/>
    </h:outputLink>

    <h:outputLink          id="javaLocales"
                        value="list/supportedLocales.remote"
                       target="_new">
      <h:outputText     value="#{messages['usecases.locales']}"/>
    </h:outputLink>

  </h:panelGrid>

  <h1><h:outputText     value="#{messages['usecases.jsp']}"/></h1>

  <h:panelGrid        columns="1">

    <h:commandLink        id="jspCategories"
                      action="lookup$listCategories">
      <h:outputText     value="#{messages['usecases.categories']}"/>
    </h:commandLink>

    <h:commandLink        id="jspLocales"
                      action="lookup$listLocales">
      <h:outputText     value="#{messages['usecases.locales']}"/>
    </h:commandLink>

  </h:panelGrid>

  <h1><h:outputText     value="#{messages['usecases.validator']}"/></h1>

  <h:panelGrid        columns="1">

    <h:commandLink         id="validation"
                       action="validate$test">
      <h:outputText     value="#{messages['usecases.validate']}"/>
    
    </h:commandLink>

  </h:panelGrid>

  <h1><h:outputText     value="#{messages['usecases.clay']}"/></h1>
  <h:panelGrid        columns="2">

    <h:commandLink         id="rolodex1"
                       action="rolodex$test1" title="#{messages['usecases.rolodex1.title']}">
      <h:outputText     value="#{messages['usecases.rolodex1']}"/>
    
    </h:commandLink>
    <h:commandLink action="rolodex$viewsource" title="#{messages['usecases.rolodex1.title']}">
       <f:param name="url" value="/rolodex/rolodex.jsp"/>
       <h:outputText     value="#{messages['usecases.rolodex.viewsource']}"/>
    </h:commandLink>


    <h:commandLink         id="rolodex2"
                       action="rolodex$test2" title="#{messages['usecases.rolodex2.title']}">
      <h:outputText     value="#{messages['usecases.rolodex2']}"/>
    
    </h:commandLink>
    <h:commandLink action="rolodex$viewsource" title="#{messages['usecases.rolodex2.title']}">
       <f:param name="url" value="/rolodex/hrolodex.html"/>
       <h:outputText     value="#{messages['usecases.rolodex.viewsource']}"/>
    </h:commandLink>

    <h:commandLink         id="rolodex3"
                       action="rolodex$test3" title="#{messages['usecases.rolodex3.title']}">
      <h:outputText     value="#{messages['usecases.rolodex3']}"/>
    
    </h:commandLink>
    <h:commandLink action="rolodex$viewsource" title="#{messages['usecases.rolodex3.title']}">
       <f:param name="url" value="/rolodex/xhrolodex.html"/>
       <h:outputText     value="#{messages['usecases.rolodex.viewsource']}"/>
    </h:commandLink>

    <h:commandLink         id="rolodex4"
                       action="rolodex$test4" title="#{messages['usecases.rolodex4.title']}">
      <h:outputText     value="#{messages['usecases.rolodex4']}"/>
    
    </h:commandLink>
    <h:commandLink action="rolodex$viewsource" title="#{messages['usecases.rolodex4.title']}">
       <f:param name="url" value="/rolodex/rolodex.xml"/>
       <h:outputText     value="#{messages['usecases.rolodex.viewsource']}"/>
    </h:commandLink>


  </h:panelGrid>


 </h:form>

</body>
</html>
</f:view>
