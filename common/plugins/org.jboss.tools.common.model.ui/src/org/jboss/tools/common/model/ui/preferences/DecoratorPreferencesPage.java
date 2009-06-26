/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.preferences;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * @author Viacheslav Kabanovich
 */
public class DecoratorPreferencesPage extends PreferencePage implements IWorkbenchPreferencePage {
	
	public static final String ID = "org.jboss.tools.common.model.ui.decorator"; //$NON-NLS-1$
	
	DecoratorGeneralPreferencesPage general = new DecoratorGeneralPreferencesPage();
	DecoratorTextPreferencesPage text = new DecoratorTextPreferencesPage();
	
	public DecoratorPreferencesPage() {}

	@Override
	protected Control createContents(Composite parent) {
		this.noDefaultAndApplyButton();
		TabFolder tabbedComposite = new TabFolder(parent,SWT.NULL);
		tabbedComposite.setBackground(parent.getBackground());
		
		TabItem newTab = null;

//TODO  remove comments when we find some preferences to put here
//		newTab = new TabItem(tabbedComposite,SWT.NULL);
//		general.createControl(tabbedComposite);
//		newTab.setControl(general.getControl());
//		newTab.setText(general.getTitle());

		newTab = new TabItem(tabbedComposite,SWT.NULL);

		text.createControl(tabbedComposite);
		newTab.setControl(text.getControl());
		newTab.setText(text.getTitle());

		return tabbedComposite;
	}

	public void init(IWorkbench workbench) {
		
	}

	public boolean performCancel() {
		general.performCancel();
		text.performCancel();
		
		return true;
	}

	public boolean performOk() {
		general.performOk();
		text.performOk();
		
		return super.performOk();
	}

	public void performDefaults() {
		general.performDefaults();
		text.performDefaults();
	}

	public void dispose() {
		general.dispose();
		text.dispose();
		
		super.dispose();
	}

}
