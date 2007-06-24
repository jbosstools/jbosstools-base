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
package org.jboss.tools.common.model.ui.wizards.special;

import java.util.Properties;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import org.jboss.tools.common.meta.action.impl.*;

public abstract class AbstractSpecialWizardStep implements ISpecialWizardStep {
	protected SpecialWizardSupport support = null;
	protected DefaultSpecialWizard wizard;
	protected WizardDataValidator validator;
	protected int id;
	
	public void setWizard(DefaultSpecialWizard wizard) {
		this.wizard = wizard;
	}

	public void setSupport(SpecialWizardSupport support, int i) {
		this.support = support;
		id = i;
		validator = support.getValidator(i);
	}
	
	public abstract Control createControl(Composite parent);

	public void update() {}
	public void save() {}
	public void clear() {}
	public boolean addIcon() {
		return true;
	}
	public void dispose() {
		support = null;
		wizard = null;
	}
	
	public Point getMinimumSize() {
		return null;
	}
	public Point getMaximumSize() {
		return null;
	}
	
	public void validate() {
		if(validator == null) return;
		wizard.dataChanged(validator, new Properties());
	}
	
	public boolean isDataChanged() {
		return true;
	}
	
    protected void createProgressMonitorPart(Composite composite) {
        wizard.getProgressPart().createProgressMonitorPart(composite);
    }

}
