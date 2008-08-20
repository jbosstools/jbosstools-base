/*******************************************************************************
  * Copyright (c) 2007-2008 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.model.ui.attribute.editor;

import java.beans.PropertyChangeListener;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Slider;
import org.jboss.tools.common.model.ui.IAttributeErrorProvider;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class SliderFieldEditor2 extends ExtendedFieldEditor implements
		IFieldEditor, IPropertyFieldEditor, IPropertyChangeListener,
		PropertyChangeListener {
	
	protected IPropertyEditor propertyEditor;
	
	protected IValueProvider valueProvider;
	protected IValueChangeListener valueChangeListener;
	
	private static final String SLIDER_LABEL_DEFAULT_TEXT = "50% "; //$NON-NLS-1$
	private static final int MAX_SLIDER_VALUE = 1000;
	private static final int MIN_SLIDER_VALUE = 0;
	private static final int INCREMENT_SLIDER_VALUE = 10;
	private static final int DEFAULT_SLIDER_VALUE = 500;
	private Composite composite = null;
	private Label sliderLabel = null;
	private Slider slider = null;
	private SelectionListener selectionListener = null;
	private int intValue = 0;
	
	public SliderFieldEditor2() {
		super();
	}

	public SliderFieldEditor2(IWidgetSettings settings) {
		super(settings);
	}
	
	public SliderFieldEditor2(IPropertyEditor propertyEditor) {
		super();
		this.propertyEditor = propertyEditor;
	}

	/**
	 * Initializes the slider.
	 */
	protected void init() {
		/*
		 * Initialize slider selection value with stored value
		 */
		try {
			intValue = Integer.parseInt(valueProvider.getStringValue(true));
		} catch (NumberFormatException e) {
			// Do nothing
		}
		setPropertyChangeListener(this);
		valueProvider.addValueChangeListener(this);
	}
	
	@Override
	protected void adjustForNumColumns(int numColumns) {
		if (null != composite) {
			if (null != composite.getLayoutData()) {
				((GridData)slider.getLayoutData()).horizontalSpan = numColumns;
			}
		}
	}

	@Override
	protected void doFillIntoGrid(Composite parent, int numColumns) {
		getLabelComposite(parent);
		Control control = createSpinnerControl(parent);
		GridData gd;
		gd = new GridData(GridData.FILL_HORIZONTAL);
		control.setLayoutData(gd);
	}

	@Override
	protected void doLoad() {
		if (null != slider) {
			slider.setSelection(getPreferenceStore().getInt(getPreferenceName()));
			valueChanged();
		}
	}

	@Override
	protected void doLoadDefault() {
		if (null != slider) {
			slider.setSelection(getPreferenceStore().getDefaultInt(getPreferenceName()));
			valueChanged();
		}
	}

	@Override
	protected void doStore() {
		if (null != slider) {
			getPreferenceStore().setValue(getPreferenceName(), slider.getSelection());
		}
	}

	public int getNumberOfControls() {
		return 2;
	}

	public Control[] getControls(Composite parent) {
		return new Control[] { 
				getLabelComposite(parent),
				createSpinnerControl(parent) 
				};
	}

	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (null != propertyEditor) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
			setErrorProvider((IAttributeErrorProvider)propertyEditor.getAdapter(IAttributeErrorProvider.class));
		}
		init();
	}

	public void propertyChange(PropertyChangeEvent event) {
		if(ExtendedFieldEditor.VALUE.equals(event.getProperty())) {
			setPropertyChangeListener(null);
			java.beans.PropertyChangeEvent e = new java.beans.PropertyChangeEvent(this, IPropertyEditor.VALUE, event.getOldValue(), event.getNewValue());
			valueChangeListener.valueChange(e);
			setPropertyChangeListener(this);
		}
	}

	public void propertyChange(java.beans.PropertyChangeEvent evt) {
		valueProvider.removeValueChangeListener(this);
		if (IPropertyEditor.VALUE.equals(evt.getPropertyName())) {
			Object v = evt.getNewValue();
			if (null != v) {
				this.setStringValue(v.toString());
			}
		}
		valueProvider.addValueChangeListener(this);
	}
	
	protected Control createSpinnerControl(Composite parent) {
		/*
		 * Create composite control with label and slider in it. 
		 */
		composite = new Composite(parent, SWT.NONE);
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        gridLayout.horizontalSpacing = HORIZONTAL_GAP;
		composite.setLayout(gridLayout);
		GridData gd;
		gd = new GridData(GridData.FILL_HORIZONTAL);
		composite.setLayoutData(gd);
		
		/*
		 * Create label
		 */
		sliderLabel = new Label(composite,SWT.NONE);
		gd = new GridData(SWT.LEFT);
		sliderLabel.setLayoutData(gd);
		sliderLabel.setText(SLIDER_LABEL_DEFAULT_TEXT);
		Point size = sliderLabel.computeSize(SWT.DEFAULT, SWT.DEFAULT);
		sliderLabel.setSize(size);
		
		/*
		 * Create slider
		 */
		slider = new Slider(composite, SWT.HORIZONTAL);
		gd = new GridData(SWT.RIGHT | GridData.FILL_HORIZONTAL);
		slider.setLayoutData(gd);
		
		slider.setMaximum(MAX_SLIDER_VALUE + slider.getThumb());
		slider.setMinimum(MIN_SLIDER_VALUE);
		slider.setIncrement(INCREMENT_SLIDER_VALUE);

		/*
		 * Add listener to handle value change  
		 */
		selectionListener = new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				valueChanged();
			}

			public void widgetDefaultSelected(SelectionEvent e) {
				// Do nothing
			}
		};
		slider.addSelectionListener(selectionListener);
		try {
			intValue = Integer.parseInt(valueProvider.getStringValue(true).toString());
		} catch (NumberFormatException e) {
			intValue = DEFAULT_SLIDER_VALUE;
		}
		
		/*
		 * Initialize slider presentation
		 */
		slider.setSelection(intValue);
		String weightsString = "" + (intValue /10) + "%";  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		slider.setToolTipText(weightsString);
		sliderLabel.setText(weightsString);
		
		slider.pack();
		sliderLabel.pack();
		return composite;
	}
	
	/**
	 * Notifies listeners of value change.
	 */
	protected void valueChanged() {
		int newValue = slider.getSelection();
		int oldValue = this.intValue;
		intValue = newValue;
		java.beans.PropertyChangeEvent event = new java.beans.PropertyChangeEvent(
				this, IPropertyEditor.VALUE, oldValue, newValue);
		valueChangeListener.valueChange(event);
	}
	
	/**
	 * Sets the value to slider and updates controls presentation.
	 * 
	 * @param stringValue the new slider value
	 */
	private void setStringValue(String stringValue) {
		if (null != slider) {
			try {
				intValue = Integer.parseInt(stringValue);
				slider.setSelection(intValue);
				/*
				 * Show value in percents.
				 */
				String weightsString = "" + (intValue / 10) + "%"; //$NON-NLS-1$ //$NON-NLS-2$
				slider.setToolTipText(weightsString);
				if (null != sliderLabel) {
					sliderLabel.setText(weightsString);
				}
			} catch (NumberFormatException e) {
				// Do nothing
			}
		}
	}
	
	@Override
	public void copy() {

	}

	@Override
	public void cut() {

	}

	@Override
	public void delete() {

	}

	@Override
	public void paste() {

	}
}
