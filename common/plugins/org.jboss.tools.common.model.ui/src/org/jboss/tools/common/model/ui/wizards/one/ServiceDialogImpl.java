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
package org.jboss.tools.common.model.ui.wizards.one;

import java.util.Properties;
import org.jboss.tools.common.model.ui.dialog.MessageAndCheckboxDialog;
import org.jboss.tools.common.model.ui.wizards.special.DefaultSpecialWizard;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;
import org.jboss.tools.common.meta.action.impl.XEntityDataImpl;
import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class ServiceDialogImpl implements ServiceDialog {
	protected String title;
	protected String message;
	protected String[] options;
	protected int type;
	protected XModel model;
	int returnCode = -1;
	
	public void setModel(XModel model) {
		 this.model = model;
	}
	
	private int getEclipseType(int type) {
		if(type == ERROR) return MessageDialog.ERROR;
		if(type == MESSAGE) return MessageDialog.INFORMATION;
		if(type == WARNING) return MessageDialog.WARNING;
		if(type == QUESTION) return MessageDialog.QUESTION;
		return MessageDialog.NONE;		
	}
	
	public static Shell getShell() {
		try {
			return ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell();
		} catch (Exception t) {
			return null;
		}
		
	}
	
	public int showDialog(String title, String message,
						  String[] options, XEntityData data, int type) {
		this.title = title;
		this.message = message;
		this.options = options;
		this.type = type;
		if(data == null) {
			MessageDialog d = new MessageDialog(getShell(), title, null, message, getEclipseType(type), options, 0);
			d.create();
			return d.open();
		}		
		SpecialWizardSupport support = new SpecialWizardSupportImpl();
		XModelObject target = model.getRoot();
		if(data == null) data = XEntityDataImpl.create(new String[][]{{model.getRoot().getModelEntity().getName()}}); 
		support.setActionData(null, new XEntityData[]{data}, target, null);
		returnCode = -1;
		showDialog(support);
		return returnCode;
	}

	class SpecialWizardSupportImpl extends SpecialWizardSupport {

		public String getTitle() {
			return title;
		}

		public String getMessage(int stepId) {
			return message;
		}
		
		public String[] getActionNames(int stepId) {
			return options;
		}
		
		public void action(String name) throws Exception {
			for(int i = 0; i < options.length; i++) {
				if(name.equals(options[i])) {
					returnCode = i;
					setFinished(true);
					return;
				}
			}
		}
	
	}

	public boolean openConfirm(Properties p) {		
		return MessageAndCheckboxDialog.openConfirm(getShell(), p);
	}

	public void showDialog(SpecialWizardSupport support) {
		DefaultSpecialWizard w = new DefaultSpecialWizard();
		w.setObject(new Object[]{support});
		w.execute();
	}
	
}
