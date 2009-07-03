/*
 * Created on 24.02.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package org.jboss.tools.common.model.ui.forms;

/**
 * @author Eskimo
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class FormActionData implements IFormActionData {

	private String label;
	private String actionPath;

	/**
	 * 
	 * @param label (translatable)
	 * @param actionPath (non-translatable)
	 */
	public FormActionData(String label, String actionPath) {
		this.label = label;
		this.actionPath = actionPath;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.forms.IFormActionData#getActionLabel()
	 */
	public String getActionLabel() {
		return label;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.forms.IFormActionData#getActionPath()
	 */
	public String getActionPath() {
		return actionPath;
	}
}