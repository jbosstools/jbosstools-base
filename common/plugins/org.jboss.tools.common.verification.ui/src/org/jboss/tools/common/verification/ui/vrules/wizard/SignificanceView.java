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
package org.jboss.tools.common.verification.ui.vrules.wizard;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.verification.vrules.VManager;

public abstract class SignificanceView {
	protected VManager manager;

	public void setManager(VManager manager) {
		this.manager = manager;
	}

	public Control createControl(Composite parent) {
		Composite control = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(2, false);
		layout.horizontalSpacing = 10;
		layout.marginHeight = 0;
		layout.verticalSpacing = 0;
		layout.marginWidth = 0;
		control.setLayout(layout);
		GridData gd = new GridData(GridData.FILL_BOTH);
		control.setLayoutData(gd);
		
		Label label = new Label(control, SWT.NONE);
		label.setText("Verification Level: ");
//		Control sc = 
			createSignificanceControl(control);
		return control;
	}
	
	protected abstract Control createSignificanceControl(Composite parent);
	
	public String getMinSignificancePresentation(int i) {
		return (i == 0) ? "any" : (i == 9) ? "only 10" : "greater than " + i;
	}
	
}
