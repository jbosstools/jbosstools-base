/*
 * Created on 22.07.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package org.jboss.tools.common.model.ui.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/**
 * @author eskimo
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class MessageWizardPage extends WizardPage{
	String message = "";
	String title = "";
	public MessageWizardPage(String title,String message) {
		super(message);
		this.message=message;
		this.title = title;
	}
	public void createControl(Composite parent) {
		Label l = new Label(parent, SWT.NONE);
		l.setText(message);
		setControl(l);
		this.setTitle(title);
		setPageComplete(false);
	}

}
