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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;

import org.eclipse.jface.dialogs.IDialogConstants;
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
import org.eclipse.swt.widgets.Layout;

import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.widgets.DefaultSettings;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class DefaultButtonControl implements IButtonControl {
	
	private PropertyChangeSupport pcs = new PropertyChangeSupport(this); 
	private Composite composite;
	private IWidgetSettings settings;
	private ArrayList<String> actions = new ArrayList<String>();
	private HashMap<String,ButtonWrapper> buttons = new HashMap<String,ButtonWrapper>();
	private Layout layout;

	public void setLayout(Layout layout) {
		this.layout = layout;
	}

	public void setWidgetSettings(IWidgetSettings settings) {
		this.settings = settings;
	}

	public void addButton(String action, String label) {
		actions.add(action);
		buttons.put(action, new ButtonWrapper(action, label));
		if (this.composite!=null) {
			// add button to composite
			createButton(composite, (ButtonWrapper)buttons.get(action));
			refresh();
		} 
	}

	public void removeButton(String action) {
		actions.remove(action);
		buttons.remove(action);
		if (this.composite!=null) {
			// remove button from composite
			((ButtonWrapper)buttons.get(action)).dispose();
			refresh();
		} 
	}

	public void clear() {
		// dispose buttons
		Iterator i = actions.iterator();
		while (i.hasNext()) {
			ButtonWrapper buttonWrapper = ((ButtonWrapper)buttons.get(i.next()));
			buttonWrapper.dispose();
		}
		refresh();
		// clear HashSet
		actions.clear();
		buttons.clear();
	}

	public Control createControl(Composite parent) {
		if (composite==null) {
			composite = new Composite(parent, SWT.NONE);
			
			composite.setBackgroundMode(SWT.INHERIT_DEFAULT);
//			composite.setBackground(parent.getBackground());

			if (layout==null) layout = createDefaultLayout();
			composite.setLayout(layout);
			
			if (buttons.size()>0) {
				// add all buttons
				Iterator i = actions.iterator();
				while (i.hasNext()) {
					Object object = i.next();
					ButtonWrapper buttonWrapper = (ButtonWrapper)buttons.get(object);
					if (buttonWrapper!=null) { 
						createButton(composite, buttonWrapper);
					} else {
						if(ModelUIPlugin.getDefault().isDebugging()) {						
							ModelUIPlugin.log("No button for action: "+object);
						} 
					}
				}
			} else {
				// set composite width equals button width + margins
				int margins = 0;
				if (layout instanceof GridLayout) {
					margins += ((GridLayout)layout).marginWidth*2;
				}
				GridData gd = new GridData();
				gd.widthHint = margins + convertHorizontalDLUsToPixels(composite, IDialogConstants.BUTTON_WIDTH);
				composite.setLayoutData(gd);
			}
		}
		return composite;
	}
	
	protected Layout createDefaultLayout() {
		GridLayout layout = new GridLayout();
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		layout.horizontalSpacing = 5;
		layout.verticalSpacing = 5;
		return layout;
	}
	
	protected Control createButton(Composite parent, ButtonWrapper buttonWrapper) {
		int style = getSettings().getStyle("Button.Style");
		if (style == SWT.DEFAULT) style = SWT.PUSH;
		Control buttonControl = buttonWrapper.createControl(parent, style);
		// widget settings
		Color bg = getSettings().getColor("Button.Background");
		Color fg = getSettings().getColor("Button.Foreground");
		Font font = getSettings().getFont("Button.Font");
		buttonControl.setBackground(bg);
		buttonControl.setForeground(fg);
		buttonControl.setFont(font);
		// layout data
		GridData gd = new GridData();
		gd.widthHint = convertHorizontalDLUsToPixels(buttonControl, IDialogConstants.BUTTON_WIDTH);
		buttonControl.setLayoutData(gd);
		//
		return buttonControl;		 		
	}

	protected void refresh() {
		Collection collection;
		Iterator i;
		if ((composite==null)||(composite.isDisposed())) return;
		// setRedraw(false) 
		collection = buttons.values();
		i = collection.iterator();
		while (i.hasNext()) {
			ButtonWrapper buttonWrapper = (ButtonWrapper)i.next();
			buttonWrapper.getControl().setRedraw(Boolean.FALSE.booleanValue());
		}
		composite.setRedraw(Boolean.FALSE.booleanValue());
		// layout();		
		composite.layout(Boolean.TRUE.booleanValue());
		// setRedraw(true) 
		collection = buttons.values();
		i = collection.iterator();
		while (i.hasNext()) {
			ButtonWrapper buttonWrapper = (ButtonWrapper)i.next();
			buttonWrapper.getControl().setRedraw(Boolean.TRUE.booleanValue());
		}
		composite.setRedraw(Boolean.TRUE.booleanValue());
	}

	public Control getControl() {
		return composite;
	}

	public void setEnabled(String action, boolean enabled) {
		((ButtonWrapper)buttons.get(action)).setEnabled(enabled);
	}

	public void dispose() {
		// remove all buttons
		clear();
		// dispose component
		composite.dispose();	
		composite = null;	
		// clear listeners
	}

	public void addPropertyChangeListener(PropertyChangeListener listener) {
		pcs.addPropertyChangeListener(listener);
	}

	public void removePropertyChangeListener(PropertyChangeListener listener) {
		pcs.removePropertyChangeListener(listener);
	}
	
	protected void firePropertyChange(PropertyChangeEvent event) {
		pcs.firePropertyChange(event);
	}
	
	protected void doButtonPressed(Button control, String action) {
		if(ModelUIPlugin.getDefault().isDebugging()) {						
			ModelUIPlugin.log("Button pressed: "+action);
		}
		firePropertyChange(new PropertyChangeEvent(control, action, null, Boolean.TRUE));
	}
	
	class ButtonWrapper {
		private String buttonAction;
		private String buttonLabel;
		private Button buttonControl;
		private boolean enabled = Boolean.TRUE.booleanValue(); // by default
		
		private ButtonWrapper() {}
		
		public ButtonWrapper(String buttonAction, String buttonLabel) {
			this.buttonAction = buttonAction;
			this.buttonLabel = buttonLabel;
		}
		
		public Control createControl(Composite parent, int style) {
			if (buttonControl==null) {
				buttonControl = new Button(parent, style);
				buttonControl.setText(buttonLabel);
				buttonControl.setEnabled(enabled);
				buttonControl.addSelectionListener(new SelectionListener(){
					public void widgetSelected(SelectionEvent e) {
						doButtonPressed(buttonControl, buttonAction);
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
			buttonLabel = null;
		}
		public void setEnabled(boolean enabled) {
			this.enabled = enabled;
			if (buttonControl!=null) buttonControl.setEnabled(enabled);
		}
	}

	protected int convertHorizontalDLUsToPixels(Control control, int dlus) {
		GC gc= new GC(control);
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

	public IWidgetSettings getSettings() {
		if (settings==null) settings = DefaultSettings.getDefault();
		return settings;
	}

	public void setSettings(IWidgetSettings settings) {
		this.settings = settings;
	}

}
