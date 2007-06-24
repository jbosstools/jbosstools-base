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
package org.jboss.tools.common.model.ui.attribute.adapter.custom;

import java.lang.reflect.InvocationTargetException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.common.model.ui.*;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.eclipse.jdt.core.IJavaModel;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.search.*;
import org.eclipse.jdt.ui.*;
import org.eclipse.jface.operation.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.dialogs.SelectionDialog;

import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class EclipseClassChooserAdapter extends DefaultValueAdapter implements IActionHelper {	

	public String invoke(Control control) {
		if(busy) return null;
		IRunnableContext context = new RC(control);
		try {
			IJavaModel jm = (IJavaModel)JavaCore.create(ModelUIPlugin.getWorkspace().getRoot());
			IJavaSearchScope scope = SearchEngine.createJavaSearchScope(jm.getJavaProjects());
			
			int flags = IJavaElementSearchConstants.CONSIDER_CLASSES;
			SelectionDialog dialog = JavaUI.createTypeDialog(control.getShell(), context, scope, flags, false, getStringValue(true));
			dialog.create();
			if(getAttribute() != null) {
				String title = "Edit " + WizardKeys.getAttributeDisplayName(getAttribute(), true);
				dialog.getShell().setText(title);
			} else {
				dialog.getShell().setText("Edit");
			}
			int q = dialog.open();
			if(q != SelectionDialog.OK) return null;
			Object[] os = dialog.getResult();
			if(os == null) return null;
			for (int i = 0; i < os.length; i++) {
				IType type = (IType)os[i];
				return type.getFullyQualifiedName();
			}
		} catch (Exception e) {
			ModelUIPlugin.log(e);
		}
		return null;			
	}
	
	public String getCommand() {
		return "...";
	}

	public Object getAdapter(Class adapter) {
		if (adapter == IActionHelper.class) return this;
		return super.getAdapter(adapter);
	}
	
	boolean busy = false;
	
	class RC implements IRunnableContext {
		Control control;
		RC(Control control) {
			this.control = control;
		}
		public void run(boolean fork, boolean cancelable, IRunnableWithProgress runnable) throws InvocationTargetException, InterruptedException {
			busy = true;
			control.setEnabled(false);
			try {
				ModalContext.run(runnable, fork, new NullProgressMonitor(), Display.getCurrent());
			} finally {
				busy = false;
				if(!control.isDisposed()) control.setEnabled(true);
			}
		}
	}
	
	
}
