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
<%@include              file="../messages.jspf"%>
<html>
<head>
<title>
  <h:outputText        value="#{messages['validate.test.title']}"/>
</title>
</head>
<body>

  <h:form                 id="form" 
                    onsubmit="return validateForm(this);">

    <h:outputText      value="#{messages['validate.test.payment.heading']}"
                       style="font-size: 1.2em;"/>
    <p>
    <h:panelGrid          id="grid"
                     columns="3">

      <h:outputLabel     for="amount"    
                       value="#{messages['validate.test.amount']}"/>

      <h:outputText value="#{messages['validate.test.currency.symbol']}"/>

      <h:panelGroup>
         <h:inputText     id="amount"
                       value="#{validate$test.amount}"
                        size="7">

            <f:convertNumber 
           minFractionDigits="2"/> 

            <s:commonsValidator 
                        type="floatRange"
                         min="10" 
                         max="1000" 
                         arg="#{msgs.amount}"
                      server="true" 
                      client="false"/>

         </h:inputText>

         <h:message      for="amount"
                       style="color: red; font-style: italic;"/>
      </h:panelGroup>

      <h:outputLabel     for="creditCard"    
                       value="#{messages['prompt.creditCardNumber']}"/>

      <h:outputText    value=""/>

      <h:panelGroup>
         <h:inputText     id="creditCard"
                        size="11"
                       value="#{validate$test.creditCard}">

            <s:commonsValidator 
                        type="required"
                         arg="#{messages['prompt.creditCardNumber']}"
                     server="true" 
                     client="false"/>

            <s:commonsValidator 
                        type="creditCard"
                         arg="#{messages['prompt.creditCardNumber']}"
                      server="true" 
                      client="false"/>

            <s:commonsValidator type="mask" 
                       mask="[4-6].*"
                    message="#{messages['validate.test.unknown.credit.card.type']}"
                     server="false" 
                     client="true"/>

         </h:inputText>

         <h:message      for="creditCard"
                       style="color: red; font-style: italic;"/>
      </h:panelGroup>

      <h:outputText value="#{messages['prompt.expirationDate']}"/>

      <h:outputText    value=""/>

      <h:panelGroup>
        <h:inputText     id="date"
                      value="#{validate$test.expirationDate}">

          <f:convertDateTime 
                     pattern="MM/dd/yyyy"/>

            <s:commonsValidator 
                        type="required"
                        arg="#{messages['prompt.expirationDate']}"
                     server="false" 
                     client="true"/>

          <s:commonsValidator  
                       type="date"
          datePatternStrict="MM/dd/yyyy" 
                    message="#{messages['validate.test.bad.expiration.date']}"
                        arg="#{messages['prompt.expirationDate']}"
                     server="false" 
                     client="true"/> 

       </h:inputText>
       
       <h:message       for="date"
                       style="color: red; font-style: italic;"/>

      </h:panelGroup>

      <h:outputText    value=""/>
      <h:outputText    value=""/>
      <h:outputText    value=""/>

      <h:commandButton value="Submit"
                      action="validate$thankYou"/>

    </h:panelGrid>

	 <f:verbatim><p></f:verbatim>

	 <h:outputText     value="#{messages['validate.test.description']}"
	                  escape="false"
	                   style="font-style: italic"/>

    <f:verbatim><p></f:verbatim>

   <h:commandLink 
                 immediate="true"  
                     value="Use cases top-level menu"
                    action="usecases$toplevel"/>

    <s:validatorScript 
	            functionName="validateForm"/>

  </h:form>

</body>
</html>
</f:view>
