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

import java.beans.PropertyChangeEvent;
import java.util.Properties;

import org.jboss.tools.common.model.ui.attribute.XAttributeSupport;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import org.jboss.tools.common.meta.action.XAttributeData;
import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;

public class SpecialWizardStep extends AbstractSpecialWizardStep implements java.beans.PropertyChangeListener {
	protected XAttributeSupport attributes = new XAttributeSupport();
	protected boolean isDataChanged = false;
	protected Composite stepControl = null;

	public SpecialWizardStep() {}

	public void dispose() {
		if(wizard != null) {
			wizard.getProgressPart().dispose();
		}
		super.dispose();
		if (attributes != null) attributes.dispose();
		attributes = null;
	}
	
	public void setSupport(SpecialWizardSupport support, int i) {
		super.setSupport(support, i);
		if(attributes == null) attributes = new XAttributeSupport();
		attributes.removePropertyChangeListener(this);
		attributes.init(support.getTarget(), support.getEntityData()[i]);
		attributes.addPropertyChangeListener(this);
	}

	public Control createControl(Composite parent) {
		stepControl = attributes.createControl(parent);
		String focusAttr = support.getFocusAttribute(id);
		if(focusAttr != null && attributes.getFieldEditorByName(focusAttr) != null) {
			attributes.getFieldEditorByName(focusAttr).setFocus();
		}
     /* Add Progress Monitor only when may be needed
      * If there appears such a case an interface method
      * will be added support.needsProgressMonitor(id);
      */
///		createProgressMonitorPart(parent);
		updateFieldEnablement();
		return stepControl;
	}

	public void save() {
		if(attributes != null) attributes.store();
	}

	public void clear() {
		if(attributes != null) attributes.load();
	}
	
	public Point getMinimumSize() {
		return null;
	}
	public Point getMaximumSize() {
		return null;
	}

	public void propertyChange(PropertyChangeEvent arg0) {
		isDataChanged = true;
		attributes.store();
		validate();
		updateFieldEnablement();
	}
	
	public void validate() {
		if(validator == null || attributes == null) return;
		wizard.dataChanged(validator, attributes.getValues());
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
	
}
