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
package org.jboss.tools.common.model.ui.action;

import java.util.*;
import org.jboss.tools.common.model.ui.wizards.OneStepWizard;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.meta.action.SignificanceMessageFactory;
import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.meta.action.XActionItem;
import org.jboss.tools.common.meta.action.XRedirect;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.event.ActionDeclinedException;
import org.jboss.tools.common.reporting.ProblemReportingHelper;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class XModelObjectAction extends XModelObjectActionItem {
	protected XAction action;
	ActionX eclipseAction;
	
	public XModelObjectAction(XAction action, XModelObject object, XModelObject[] targets, Object environment) {
		super((XActionItem)action, object, targets, environment);
		this.action = action;
		eclipseAction = new ActionX(this);
	}
	
	public Action getEclipseAction() {
		return eclipseAction;
	}
	
	public Shell getShell() {
		if(shell != null) return shell;
		try {
			return ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell();
		} catch (Exception t) {
			return null;
		}
		
	}
	public void actionPerformed() {
		try {
			XRedirect redirect = action.getRedirect();
			XAction redirectAction = null;
			XModelObject redirectObject = null;
			if(redirect != null) {
				redirectAction = redirect.getRedirectAction(object);
				redirectObject = redirect.getRedirectSource(object);
			}			
			XAction runAction = action;
			XModelObject runObject = object;
			if(redirectAction != null && redirectObject != null) {
				runAction = redirectAction;
				runObject = redirectObject;
			}
			if(action.getSignificantFlag(object)) {
				String message = SignificanceMessageFactory.getInstance().getMessage(action, object, targets) + "?";
				MessageDialog d = new MessageDialog(getShell(), "Confirmation", null, message, MessageDialog.QUESTION, new String[]{"OK", "Cancel"}, 0);
				int m = d.open();
				if(m != 0) return;
			}
			String wizardName = runAction.getWizardClassName();
			if(wizardName == null || wizardName.equals("")) {
				action.getEntityData(object);
				try {
					Properties p = prepareProperties();
					if(p == null) p = new Properties();
					if(getShell() != null) p.put("shell", getShell());
					if(targets == null) action.executeHandler(object, p);
					else action.executeHandler(object, targets, p);
				} catch (ActionDeclinedException e) {
					return;
				}
			} else {
				runAction.getEntityData(runObject);
				OneStepWizard w = new OneStepWizard();
				Properties p = prepareProperties();
				if(p == null) p = new Properties();
				p.put("action", runAction);
				p.put("object", runObject);
				if(getShell() != null) p.put("shell", getShell());
				w.setObject(p);
				w.execute();
		   }
		} catch (ActionDeclinedException de) {
		} catch(Exception e) {
			ProblemReportingHelper.reportProblem(ModelUIPlugin.PLUGIN_ID, e);
		} 
	}
	
	protected Properties prepareProperties() {
		if(environment == null) return null;
		if(environment instanceof Properties) return (Properties)environment;
		if(!(environment instanceof Object[])) return null;
		Object[] os = (Object[])environment;
		for (int i = 0; i < os.length; i++) {
			if(os[i] instanceof Properties ) return (Properties)os[i];
		}
		return null;
	}
	
	class AL implements SelectionListener {
		public void widgetSelected(SelectionEvent e) {
			actionPerformed();
		}
		public void widgetDefaultSelected(SelectionEvent e) {}
	}
	
	public void createMenuItem(Menu menu) {
		boolean enabled = (targets == null) ? action.isEnabled(object) : action.isEnabled(object, targets);
		boolean hidden = action.hide(enabled) || (!enabled && targets != null);
		if(hidden) return;
		MenuItem item = new MenuItem(menu, SWT.CASCADE);
		item.setAccelerator(eclipseAction.getAccelerator());
		item.addSelectionListener(new AL());
		item.setText(eclipseAction.getText());
		item.setEnabled(eclipseAction.isEnabled());
		item.setImage(eclipseAction.getImageDescriptor().createImage());
	}
	
}
