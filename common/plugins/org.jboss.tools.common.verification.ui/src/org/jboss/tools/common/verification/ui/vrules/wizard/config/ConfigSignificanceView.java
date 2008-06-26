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
package org.jboss.tools.common.verification.ui.vrules.wizard.config;

import org.eclipse.swt.widgets.*;

import org.jboss.tools.common.verification.ui.vrules.wizard.SignificanceView;

public class ConfigSignificanceView extends SignificanceView {
	Combo combo;
	String[] values;
	
	public ConfigSignificanceView() {
		 values = new String[10];
		 for (int i = 0; i < 10; i++) values[i] = getMinSignificancePresentation(i);
	}
	
	public int getSignificance() {
		return combo.getSelectionIndex();
	}
	
	protected Control createSignificanceControl(Composite parent) {
		combo = new Combo(parent, 0);
		for (int i = 0; i < 10; i++) combo.add(values[i]);
		return combo;
	}
	
	public void loadDefaults() {
		manager.setMinSignificance(0);
		update();
	}
	
	public void update() {
		int i = manager.getMinSignificance();
		if(i >= 0 && i < values.length) combo.select(i);
	}

	public void commit() {
		manager.setMinSignificance(getSignificance());
	}

}
