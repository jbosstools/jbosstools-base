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
package org.jboss.tools.common.model.ui.wizards.query;

import java.util.Properties;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.meta.action.SpecialWizard;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class AbstractQueryWizard implements SpecialWizard {
	private AbstractQueryWizardView view = null;

	public void dispose() {
		if (view!=null) view.dispose();
		view = null;
	}

	public void setView(AbstractQueryWizardView view) {
		this.view = view;
	}
	
	public AbstractQueryWizardView getView() {
		return view;
	}

	public void setObject(Object object) {
		view.setModel(findModel(object));
		view.setObject(object);
		Properties p = findProperties(object);
		String key = (p == null) ? null : p.getProperty("help");
		if(key == null) {
			//put debuggin here
		} else {
			String title = p.getProperty("title");
			if(title == null) title = WizardKeys.getHeader(key);
			if(title == null) title = "Title is not found for key \"" + key + "\"";
			view.setWindowTitle(title);
			String subtitle = p.getProperty("subtitle");
			if(subtitle == null) subtitle = WizardKeys.getTitle(key);
			if(subtitle == null) subtitle = "Subtitle is not found for key \"" + key + "\"";
			view.setTitle(subtitle);
			String message = p.getProperty("message");
			if(message == null) WizardKeys.getString(key + ".Message");
			if(message != null) view.setMessage(message);
		}		
	}

	private Properties findProperties(Object o) {
		if (o instanceof Properties)
			return (Properties) o;
		if (!(o instanceof Object[]))
			return null;
		Object[] os = (Object[]) o;
		for (int i = 0; i < os.length; i++)
			if (os[i] instanceof Properties)
				return (Properties) os[i];
		return null;
	}

	private XModel findModel(Object o) {
		if(o instanceof Object[]) {
			Object[] os = (Object[])o;
			for (int i = 0; i < os.length; i++) {
				if(os[i] instanceof XModel) return (XModel)os[i];
				if(os[i] instanceof XModelObject) return ((XModelObject)os[i]).getModel();
			}
		}
		Properties p = findProperties(o);
		if(p != null) {
			Object q = p.get("model");
			if(q instanceof XModel) return (XModel)q;
			q = p.get("object");
			if(q instanceof XModelObject) return ((XModelObject)q).getModel();
		}
		return null;
	}

	public int execute() {
		Shell shell = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell();
		IQueryDialog dialog = createDialog(shell);
		dialog.setView(view);
		dialog.getDialog().create();
		view.setDialog(dialog.getDialog());
		dialog.getDialog().open();
		return view.code();
	}
	
	protected IQueryDialog createDialog(Shell shell) {
		return new AbstractQueryDialog(shell);
	}
	
}
