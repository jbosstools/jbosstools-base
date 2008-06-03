/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.wizard.newfile;

import java.beans.*;
import org.jboss.tools.common.model.ui.attribute.XAttributeSupport;
import org.eclipse.jface.wizard.*;
import org.eclipse.swt.widgets.*;

public class NewFileWizardPage extends WizardPage {
	protected XAttributeSupport support;
	protected NewFileContext context;
	
	public NewFileWizardPage(NewFileContext context) {
		super(context.getWindowTitle());
		this.context = context;
		support = new XAttributeSupport(context.getSupport().getTarget(), context.getSupport().getEntityData()[0]);
		setErrorMessage(null);
		setTitle(context.getTitle());
		setMessage(null);
	}

	public void dispose() {
		super.dispose();
		if (support!=null) support.dispose();
		support = null;
	}

	public void createControl(Composite parent) {
		Control control = support.createControl(parent);
		setControl(control);
		support.addPropertyChangeListener(new PCL());
		validatePage();
	}
	
	public void update() {
		if(context.update()) {
			support.load();
			validatePage();
		}
	}
	
	private void validatePage() {
		support.save();
		String message = context.validate(support.getValues());
		setErrorMessage(message);
		setPageComplete(message == null);		
	}
	
	class PCL implements PropertyChangeListener {
		public void propertyChange(PropertyChangeEvent evt) {
			validatePage();
		}
	}

}
