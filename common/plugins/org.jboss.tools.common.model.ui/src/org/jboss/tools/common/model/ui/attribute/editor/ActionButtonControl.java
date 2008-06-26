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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import org.jboss.tools.common.model.ui.actions.IActionProvider;
import org.jboss.tools.common.model.ui.widgets.DefaultSettings;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class ActionButtonControl implements IPropertyFieldEditor {
	
	private Composite composite;
	private ArrayList<ButtonWrapper> buttons;
	private IPropertyEditor propertyEditor;
	private IActionProvider actionProvider;
	private IWidgetSettings settings = new DefaultSettings();
	
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		initialize();
	}
	
	public Control getControl() {
		return composite;
	}
	
	public Control createControl(Composite parent) {
		if (composite==null) {
			composite = new Composite(parent, SWT.NONE);

			composite.setBackgroundMode(SWT.INHERIT_DEFAULT);

			GridLayout layout = new GridLayout();
			layout.horizontalSpacing = 5;
			layout.verticalSpacing = 5;
			layout.marginHeight = 0;
			layout.marginWidth = 0;
			composite.setLayout(layout);
			
			for (int i=0;i<buttons.size();++i) {
				createButton(composite, (ButtonWrapper)buttons.get(i));
			}
		}
		return composite;
	}
	
	private Control createButton(Composite parent, ButtonWrapper buttonWrapper) {
		int style = getSettings().getStyle("Button.Style");
		if (style==SWT.DEFAULT) style = SWT.NONE;
		if (style == 0) style = SWT.PUSH;
		Control buttonControl = buttonWrapper.createControl(parent, style);
		// widget settings
		Color fg = getSettings().getColor("Button.Foreground");
		Font font = getSettings().getFont("Button.Font");

		buttonControl.setForeground(fg);
		buttonControl.setFont(font);
		// layout data
		GridData gd = new GridData();
		gd.widthHint = convertHorizontalDLUsToPixels(buttonControl, IDialogConstants.BUTTON_WIDTH);
		buttonControl.setLayoutData(gd);
		return buttonControl;		 		
	}
	
	private void initialize() {
		if (propertyEditor!=null) {
			actionProvider = (IActionProvider)propertyEditor.getAdapter(IActionProvider.class);
			if (actionProvider==null) return;
			List actions = Arrays.asList(actionProvider.getActions());
			Iterator i = actions.iterator();
			ButtonWrapper buttonWrapper;
			buttons = new ArrayList<ButtonWrapper>();
			while(i.hasNext()) {
				buttonWrapper = new ButtonWrapper((IAction)i.next());
				buttons.add(buttonWrapper);
			}
		}
	}
	
	class ButtonWrapper implements IPropertyChangeListener {
		private IAction buttonAction;
		private Button buttonControl;
		private boolean enabled = Boolean.FALSE.booleanValue(); // by default
		
		private ButtonWrapper() {}
		
		public ButtonWrapper(IAction buttonAction) {
			this.buttonAction = buttonAction;
			this.enabled = buttonAction.isEnabled();
			buttonAction.addPropertyChangeListener(this);
		}
		
		public Control createControl(Composite parent, int style) {
			if (buttonControl==null) {
				buttonControl = new Button(parent, style);
				buttonControl.setText(buttonAction.getText()!=null?buttonAction.getText():"%text%");
				buttonControl.setEnabled(enabled);
				buttonControl.addSelectionListener(new SelectionListener(){
					public void widgetSelected(SelectionEvent e) {
						buttonAction.run();
					}
					public void widgetDefaultSelected(SelectionEvent e) {
					}
				});
			}
			return buttonControl;
		}
		public Control getControl() {
			return buttonControl;
		}
		public void dispose() {
			buttonControl.dispose();
			buttonControl = null;
			buttonAction = null;
		}
		public void setEnabled(boolean enabled) {
			this.enabled = enabled;
			if (buttonControl!=null) buttonControl.setEnabled(enabled);
		}

		public void propertyChange(PropertyChangeEvent event) {
			if (IAction.ENABLED.equals(event.getProperty())) {
				this.enabled = ((Boolean)event.getNewValue()).booleanValue();
				if (buttonControl!=null && !buttonControl.isDisposed()) {
					buttonControl.setEnabled(this.enabled);				
				}
			}
		}
	}

	public IWidgetSettings getSettings() {
		return settings;
	}

	public void setSettings(IWidgetSettings settings) {
		this.settings = settings;
	}

	protected int convertHorizontalDLUsToPixels(Control control, int dlus) {
		GC gc = new GC(control);
		gc.setFont(control.getFont());
		int averageWidth= gc.getFontMetrics().getAverageCharWidth();
		gc.dispose();
	
		double horizontalDialogUnitSize = averageWidth * 0.25;
	
		return (int)Math.round(dlus * horizontalDialogUnitSize);
	}
	
	protected int convertVerticalDLUsToPixels(Control control, int dlus) {
		GC gc= new GC(control);
		gc.setFont(control.getFont());
		int height = gc.getFontMetrics().getHeight();
		gc.dispose();
	
		double verticalDialogUnitSize = height * 0.125;
		
		return (int)Math.round(dlus * verticalDialogUnitSize);
	}
	
	public void setEnabled(boolean enabled) {
		IAction[] actions = actionProvider.getActions();
		if (actions!=null && actions.length>0) {
			for (int i=0;i<actions.length;++i) actions[i].setEnabled(enabled);
		}
	}

}
