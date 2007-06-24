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
package org.jboss.tools.common.model.ui.attribute.editor;

import org.jboss.tools.common.model.ui.IAttributeErrorProvider;
import org.jboss.tools.common.model.ui.navigator.LabelDecoratorImpl;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;

import org.jboss.tools.common.model.ui.widgets.DefaultSettings;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.widgets.WhiteSettings;

public abstract class ExtendedFieldEditor extends org.eclipse.jface.preference.FieldEditor {

	public static final String IS_VALID = "field_editor_is_valid";//$NON-NLS-1$
	public static final String VALUE = "field_editor_value";//$NON-NLS-1$
	protected static final int HORIZONTAL_GAP = 8;
	private IPreferenceStore preferenceStore = null;
	private boolean isDefaultPresented = false;
	private Label label;
	private boolean enabled = Boolean.TRUE.booleanValue();
	private IAttributeErrorProvider errorProvider;
	//
	protected IWidgetSettings settings;
	
	public ExtendedFieldEditor() {}

	public ExtendedFieldEditor(IWidgetSettings settings) {
		this.settings = settings;
	}

	protected abstract void adjustForNumColumns(int numColumns);
	
	protected void applyFont() {}
	
	protected void checkParent(Control control, Composite parent) {
		Assert.isTrue(control.getParent() == parent, "Different parents");//$NON-NLS-1$
	}

	protected int convertHorizontalDLUsToPixels(Control control, int dlus) {
		GC gc= new GC(control);
		gc.setFont(control.getFont());
		int averageWidth= gc.getFontMetrics().getAverageCharWidth();
		gc.dispose();
	
		double horizontalDialogUnitSize = averageWidth * 0.25;
	
		return (int)Math.round(dlus * horizontalDialogUnitSize);
	}
	
	protected Composite labelComposite;
	
	public Control getLabelComposite() {
		return labelComposite; //label;
	}

	protected Label getLabelControl() {
		return label;
	}
	
	public Control getLabelComposite(Composite parent) {
		return createLabelComposite(parent);
	}	

	public Label getLabelControl(Composite parent) { // super method
		getLabelComposite(parent);
		return getLabelControl(); //createLabelControl(parent);
	}
	
	protected Label errorSymbolLabel;
	
	public Control createLabelComposite(Composite parent) {
		if(getLabelComposite() == null) {
			labelComposite = new Composite(parent, SWT.NONE);
			if(settings instanceof WhiteSettings) {
				labelComposite.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_WHITE));
			}
			labelComposite.setBackgroundMode(SWT.INHERIT_DEFAULT);
			GridLayout layout = new GridLayout(2, false);
			layout.marginHeight = 0;
			layout.marginWidth = 0;
			layout.horizontalSpacing = 2;
			labelComposite.setLayout(layout);

			Color fg = getSettings().getColor("Label.Foreground");
///			Color bg = parent.getBackground();
				///getSettings().getColor("Label.Background");
///			labelComposite.setBackground(bg);

			createLabelControl(labelComposite);
			errorSymbolLabel = new Label(labelComposite, SWT.NONE);
///			errorSymbolLabel.setBackground(bg);
			errorSymbolLabel.setImage(LabelDecoratorImpl.emptyImage);
			errorSymbolLabel.setForeground(fg);
			if(settings instanceof WhiteSettings) {
				errorSymbolLabel.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_WHITE));
			}
		} else {
			checkParent(labelComposite, parent);
		}
		return labelComposite;
	}
	
	protected Label createLabelControl(Composite parent) {
		if (getLabelControl() == null) { // cannot comment this! for details see label.addDisposeListener
			int style = getSettings().getStyle("Label.Style");
///			Color bg = parent.getBackground();
				///getSettings().getColor("Label.Background");
			Color fg = getSettings().getColor("Label.Foreground");
			Font font = getSettings().getFont("Label.Font");
			if (style==SWT.DEFAULT) style = SWT.NONE;
			style |= SWT.RIGHT;
			label = new Label(parent, style);
			label.setFont(font);
///			label.setBackground(bg);
			label.setForeground(fg);
			if(settings instanceof WhiteSettings) {
				label.setForeground(new Color(null, 10, 36, 106));
				label.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_WHITE));
			}
			
			label.setEnabled(isEnabled());
			String text = getLabelText();
			if (text != null)
				label.setText(text);
			label.addDisposeListener(new DisposeListener() {
				public void widgetDisposed(DisposeEvent event) {
					label = null;
				}
			});
		} else {
			checkParent(label, parent);
		}
		return label;
	}

	public IPreferenceStore getPreferenceStore() {
		return preferenceStore;
	}

	public void dispose() {
		if (label!=null && !label.isDisposed()) label.dispose();
		setPropertyChangeListener(null);
		label = null;
		setPropertyChangeListener(null);
		setPage(null);
	}

	public void load() {
		if (preferenceStore != null) {
			isDefaultPresented = false;
			doLoad();
			refreshValidState();
		}
	}

	public void loadDefault() {
		if (preferenceStore != null) {
			isDefaultPresented = true;
			doLoadDefault();
			refreshValidState();
		}
	}

	public boolean presentsDefaultValue() {
		return isDefaultPresented;
	}

	public void setLabelText(String text) {
		super.setLabelText(text);
		if (getLabelControl() != null) getLabelControl().setText(text);
	}

	public void setPreferenceStore(IPreferenceStore store) {
		preferenceStore = store;
	}

	protected void setPresentsDefaultValue(boolean b) {
		isDefaultPresented = b;
	}

	public void setErrorProvider(IAttributeErrorProvider errorProvider) {
		this.errorProvider = errorProvider;
	}

	public IAttributeErrorProvider getErrorProvider() {
		return errorProvider;
	}

	public void store() {
		if (preferenceStore == null) return;
		if (isDefaultPresented) {
			preferenceStore.setToDefault(getPreferenceName());
		} else {
			doStore();
		}
	}

	protected void setButtonLayoutData(Button button) {
		
		GridData data = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
		
		// Compute and store a font metric
		GC gc = new GC(button);
		gc.setFont(button.getFont());
		FontMetrics fontMetrics = gc.getFontMetrics();
		gc.dispose();
		
		data.heightHint =  org.eclipse.jface.dialogs.Dialog.convertHorizontalDLUsToPixels(fontMetrics, 14/*IDialogConstants.BUTTON_HEIGHT*/);
		int widthHint = org.eclipse.jface.dialogs.Dialog.convertVerticalDLUsToPixels(fontMetrics,IDialogConstants.BUTTON_WIDTH);
		data.widthHint = Math.max(widthHint, button.computeSize(SWT.DEFAULT, SWT.DEFAULT, true).x);
		button.setLayoutData(data);
	}

	public void setEnabled(boolean enabled, Composite parent){
		this.setEnabled(enabled);
	}
	
	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
		if ((this.label!=null)&&(!this.label.isDisposed())) {
			label.setEnabled(this.enabled);
		}
	}
	
	public boolean isEnabled() {
		return enabled;
	}

	public IWidgetSettings getSettings() {
		if (this.settings==null) settings = DefaultSettings.getDefault();
		return settings;
	}

	public void setSettings(IWidgetSettings settings) {
		this.settings = settings;
	}

	public abstract void cut();
	public abstract void copy();
	public abstract void paste();
	public abstract void delete();
	
	Image errorStateImage = null;
	
	protected void updateErrorState() {
		if(getErrorProvider() != null && errorSymbolLabel != null && !errorSymbolLabel.isDisposed()) {
			boolean error = getErrorProvider().hasErrors();
			Image image = null;
			String tooltip = null;
			if(error) {
				image = LabelDecoratorImpl.errorImage;
				tooltip = getErrorProvider().getError();
			} else {
				image = LabelDecoratorImpl.emptyImage;
			}
			if(errorStateImage != image) {
				errorSymbolLabel.setImage(errorStateImage = image);
				errorSymbolLabel.setToolTipText(tooltip);
			}
		}
	}

}

