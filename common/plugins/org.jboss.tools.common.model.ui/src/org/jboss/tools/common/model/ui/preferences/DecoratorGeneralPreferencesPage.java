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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * @author Viacheslav Kabanovich
 */
public class DecoratorGeneralPreferencesPage extends PreferencePage implements IWorkbenchPreferencePage, IPreferencePageExt {
	
	public DecoratorGeneralPreferencesPage() {
		setTitle("General");
	}

	@Override
	protected Control createContents(Composite parent) {
		Composite g = new Composite(parent, SWT.NONE);
		g.setLayout(new GridLayout(1, false));
		Label label = new Label(g, SWT.NONE);
		label.setText("Bla-bla-bla");
		label.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		return g;
	}

	public void init(IWorkbench workbench) {
		// TODO Auto-generated method stub
		
	}

	public boolean performCancel() {
		
		return true;
	}

	public boolean performOk() {
	
		return super.performOk();
	}
	
	public void performDefaults() {
		
	}
}
