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
package org.jboss.tools.common.meta.action.impl;

import java.util.Properties;

public class MultistepWizardStep {
	protected SpecialWizardSupport support;
	protected int id;
	
	public void setSupport(SpecialWizardSupport support, int id) {
		this.support = support;
		this.id = id;
		reset();
	}
	
	public void reset() {}
	
	public int getId() {
		return id;
	}
	
	public SpecialWizardSupport getSupport() {
		return support;
	}
	
	public String getStepImplementingClass() {
		return "org.jboss.tools.common.model.ui.wizards.special.SpecialWizardStep";
	}
	
	public boolean isFieldEditorEnabled(String name, Properties p) {
		return true;
	}
}
