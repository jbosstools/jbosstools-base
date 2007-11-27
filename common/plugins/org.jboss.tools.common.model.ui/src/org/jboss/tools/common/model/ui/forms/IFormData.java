/***********************************************************************
 * Module:  IFormData.java
 * Author:  Eskimo
 * Created: Tuesday, February 24, 2004 1:56:35 PM
 * Purpose: Defines the Interface IFormData
 ***********************************************************************/

package org.jboss.tools.common.model.ui.forms;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public interface IFormData {

	public String getHeader();

	public String getDescription();

	public IFormAttributeData[] getAttributes();

	public String[] getEntities();

	public IFormActionData[] getActions();

	public IFormData[] getForms();

	public String getEntityName();

	public IWidgetSettings getWidgetSettings();

	public boolean isNotLayouredForm();

	public String getFormClassName();
}