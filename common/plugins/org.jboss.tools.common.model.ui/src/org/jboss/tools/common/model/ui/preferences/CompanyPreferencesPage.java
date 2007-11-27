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
package org.jboss.tools.common.model.ui.preferences;

import java.util.Locale;
import java.util.ResourceBundle;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;

public class CompanyPreferencesPage extends PreferencePage implements IWorkbenchPreferencePage {
	protected Control contents;

	public void createControl(final Composite parent) {
		super.createControl(parent);
		Object data = contents.getLayoutData();
		if(data instanceof GridData) {
			final GridData d = (GridData)data;
			d.widthHint = 300;
			contents.addControlListener(new ControlAdapter() {
				public void controlResized(ControlEvent e) {
					if(d.widthHint == SWT.DEFAULT) return;
					d.widthHint = SWT.DEFAULT;
					contents.getParent().update();
					contents.getParent().layout();
					contents.removeControlListener(this);
				}
			});
		}
	}

	protected Control createContents(Composite parent) {
		noDefaultAndApplyButton();
		StyledText newControl = new StyledText(parent, SWT.WRAP);
		ResourceBundle bundle = ResourceBundle.getBundle(getClass().getPackage().getName() + ".preferences", Locale.getDefault(), getClass().getClassLoader());
		newControl.setText(bundle.getString(getKey()));
		newControl.setBackground(parent.getBackground());
		newControl.setEditable(false);
		return contents = newControl;
	}
	
	protected String getKey() {
		return "REDHAT";
	}

	public void init(IWorkbench workbench) {}

}
