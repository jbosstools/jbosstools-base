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
package org.jboss.tools.common.model.ui.wizards.standard;

import java.util.*;
import java.beans.PropertyChangeEvent;

import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.attribute.XAttributeSupport;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.wizard.*;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.meta.action.XAttributeData;
import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;
import org.jboss.tools.common.meta.action.impl.WizardDataValidator;

public class DefaultStandardStep extends WizardPage implements java.beans.PropertyChangeListener {
	protected DefaultStandardWizard wizard;
	protected SpecialWizardSupport support;
	protected int id;
	protected XAttributeSupport attributes = new XAttributeSupport();
	boolean isDataChanged = false;
	protected Composite stepControl = null;
	protected WizardDataValidator validator;
	protected boolean isNextEnabled = true;
	
	boolean loading = false;
	
	public DefaultStandardStep(SpecialWizardSupport support, int id) {
		super("");
		this.support = support;
		this.id = id;
		validator = support.getValidator(id);
		attributes.removePropertyChangeListener(this);
		attributes.init(support.getTarget(), support.getEntityData()[id]);
		attributes.addPropertyChangeListener(this);
	}

	public void dispose() {
		super.dispose();
		if (attributes!=null) attributes.dispose();
		attributes = null;
	}

	public void setWizard(IWizard wizard) {
		super.setWizard(wizard);
		this.wizard = (DefaultStandardWizard)wizard;
	}

	public void createControl(Composite parent) {
		attributes.removePropertyChangeListener(this);
		attributes.init(support.getTarget(), support.getEntityData()[id]);
		attributes.addPropertyChangeListener(this);
		stepControl = attributes.createControl(parent);
		loading = true;
		try {
			attributes.load();
		} finally {
			loading = false;
		}
		setControl(stepControl);
	}

	public void setVisible(boolean visible) {
		super.setVisible(visible);
		if(visible) {
			if(support.getStepId() > id) {
				try {
					support.action(SpecialWizardSupport.BACK);
				} catch (Exception e) {
					ModelUIPlugin.getPluginLog().logError(e);
				}
			}
			setTitle(support.getSubtitle());
			loading = true;
			try {
				attributes.load();
			} finally {
				loading = false;
			}
			validateAll();
			String focusAttr = support.getFocusAttribute(id);
			if(focusAttr != null && attributes.getFieldEditorByName(focusAttr) != null) {
				attributes.getFieldEditorByName(focusAttr).setFocus();
			}
		}
	}
	
	private void validateAll() {
		attributes.store();
		try {
			validate();
			updateFieldEnablement();
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
	}

	public void propertyChange(PropertyChangeEvent arg0) {
		if(loading) return;
		isDataChanged = true;
		validateAll();
	}
	
	public void validate() {
		validator = support.getValidator(id);
		if(validator == null) return;
		wizard.dataChanged(validator, attributes.getValues());
		setPageComplete(computePageComplete());
	}
	
	private boolean computePageComplete() {
		String[] actions = support.getActionNames(id);
		boolean b = true;
		for(int i = 0; i < actions.length && b; i++) {
			if(actions[i].equals(SpecialWizardSupport.NEXT) && !support.isActionEnabled(SpecialWizardSupport.NEXT)) b = false;
			else if(actions[i].equals(SpecialWizardSupport.FINISH) && !support.isActionEnabled(SpecialWizardSupport.FINISH)) b = false;
		}
		return b;
	}
	
	public boolean isDataChanged() {
		return isDataChanged;
	}
	
	public void updateFieldEnablement() {
		if(stepControl == null || stepControl.isDisposed()) return;
		Properties p = attributes.getValues();
		XAttributeData[] ad = support.getEntityData()[id].getAttributeData();
		for (int i = 0; i < ad.length; i++) {
			String n = ad[i].getAttribute().getName();
			boolean b = support.isFieldEditorEnabled(id, n, p);
			FieldEditor f = attributes.getFieldEditorByName(n);
			if(f != null) f.setEnabled(b, stepControl);
		}
	}

	public boolean canFlipToNextPage() {
		if(!hasNextButton()) return false;
		return isPageComplete() && 
		       support.isActionEnabled(SpecialWizardSupport.NEXT)
		       && isNextEnabled;
	}
	
	private boolean hasNextButton() {
		String[] actions = support.getActionNames(id);
		for (int i = 0; i < actions.length; i++) {
			if(actions[i].equals(SpecialWizardSupport.NEXT)) return true;
		}
		return false;
	}

}
