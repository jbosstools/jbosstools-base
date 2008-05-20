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
package org.jboss.tools.common.model.ui.forms;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.widgets.Split;

public class SplitFormContainer extends DefaultFormContainer {

	int style; //WARNING : never used
	private IForm firstForm;
	private IForm secondForm;
	private Control control;

	public SplitFormContainer() {
		setLayoutData(new GridData(GridData.FILL_BOTH));
	}
	
	public SplitFormContainer(int style) {
		this.style = style;
	}

	public SplitFormContainer(IForm firstForm, IForm secondForm, int style) {
		this.style = style;
		this.firstForm = firstForm;
		this.secondForm = secondForm;
	}

	public SplitFormContainer(IForm firstForm, IForm secondForm) {
		this.style = SWT.HORIZONTAL;
		this.firstForm = firstForm;
		this.secondForm = secondForm;
	}

	public void dispose() {
		super.dispose();
		if (firstForm!=null) firstForm.dispose();
		firstForm = null;
		if (secondForm!=null) secondForm.dispose();
		secondForm = null;
		if (control!=null && !control.isDisposed()) control.dispose();
		control = null;
	}

	public void setForm(IForm form) {
		this.setFirstForm(form);
	}

	public IForm getForm() {
		return this.getFirstForm();
	}

	public void setExpandable(boolean expandable) {
	}

	public boolean isExpandable() {
		return false;
	}

	public void commitChanges(boolean onSave) {
	}

	public Control createFirstForm(Composite parent, IWidgetSettings settings) {
		return firstForm.createControl(parent, settings);
	}

	public Control createSecondForm(Composite parent, IWidgetSettings settings) {
		return secondForm.createControl(parent, settings);
	}

	public Control createControl(Composite parent, IWidgetSettings settings) {
		Composite main = new Composite(parent, settings.getStyle("Composite.Style"));
		settings.setupControl(main);
		main.setLayout(getLayout());
		main.setLayoutData(getLayoutData());
		
		Split composite = new Split(main, SWT.HORIZONTAL);
		
		GridData gd;

		// 1
		Composite fistFormComposite = new Composite(composite, settings.getStyle("Composite.Style"));
		settings.setupControl(fistFormComposite);
		fistFormComposite.setLayout(getLayout());
		gd = new GridData(GridData.FILL_BOTH);
		fistFormComposite.setLayoutData(gd);
		
		Control firstFormControl = this.createFirstForm(fistFormComposite, settings);
		gd = new GridData(GridData.FILL_BOTH);
		firstFormControl.setLayoutData(gd);
				

		// 2
		Composite secondFormComposite = new Composite(composite, settings.getStyle("Composite.Style"));
		settings.setupControl(secondFormComposite);
		secondFormComposite.setLayout(getLayout());
		gd = new GridData(GridData.FILL_BOTH);
		secondFormComposite.setLayoutData(gd);
		
		Control secondFormControl = this.createSecondForm(secondFormComposite, settings);		
		gd = new GridData(GridData.FILL_BOTH);
		secondFormControl.setLayoutData(gd);
		
		
		//composite.setBackground(new Color(null, 255,255,255));
		composite.setWeights(new int[]{30, 70});
		composite.setLayoutData(new GridData(GridData.FILL_BOTH));
		control = main;
		return control;
	}

	public boolean doGlobalAction(String actionId) {
		// as temp notify all forms
		boolean result = Boolean.FALSE.booleanValue();
		if (firstForm!=null) result &= firstForm.doGlobalAction(actionId);
		if (secondForm!=null) result &= secondForm.doGlobalAction(actionId);
		return result;
	}

	public void expandTo(Object object) {
	}

	public Control getControl() {
		return control;
	}

	public Color getHeadingBackground() {
		return null;
	}

	public Color getHeadingForeground() {
		return null;
	}

	public Image getHeadingImage() {
		return null;
	}

	public String getHeadingText() {
		return "SplitSection";
	}

	public void initialize(Object model) {
	}

	public boolean isHeadingVisible() {
		return false;
	}

	public void setFocus() {
	}

	public void setHeadingBackground(Color newHeadingBackground) {
	}

	public void setHeadingForeground(Color newHeadingForeground) {
	}

	public void setHeadingImage(Image headingImage) {
	}

	public void setHeadingVisible(boolean newHeadingVisible) {
	}

	public void setHeadingText(String heading) {
	}

	public void update() {
	}

	public static void main(String[] args) {
	}

	public IForm getFirstForm() {
		return firstForm;
	}

	public IForm getSecondForm() {
		return secondForm;
	}

	public void setFirstForm(IForm form) {
		firstForm = form;
	}

	public void setSecondForm(IForm form) {
		secondForm = form;
	}

	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		if (this.firstForm!=null) {
			this.firstForm.setEnabled(enabled);
		}
		if (this.secondForm!=null) {
			this.secondForm.setEnabled(enabled);
		}
	}
}
