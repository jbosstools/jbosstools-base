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

import java.util.Properties;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.jboss.tools.common.meta.action.SpecialWizard;
import org.jboss.tools.common.model.ui.ModelUIMessages;
import org.jboss.tools.common.model.ui.action.CommandBar;
import org.jboss.tools.common.model.ui.action.CommandBarListener;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class MutableMultipleChoiceFieldEditor extends MultipleChoiceFieldEditor implements CommandBarListener, IMutableFieldEditor {
	static String NEW = ModelUIMessages.MutableMultipleChoiceFieldEditor_New;
	static String SELECT_ALL = ModelUIMessages.MutableMultipleChoiceFieldEditor_SelectAll;
	static String DESELECT_ALL = ModelUIMessages.MutableMultipleChoiceFieldEditor_DeselectAll;

	CommandBar bar = new CommandBar();
	String[] commands = {NEW, SELECT_ALL, DESELECT_ALL};

	private Composite composite;

	private SpecialWizard change;
	
	public MutableMultipleChoiceFieldEditor() {
		this(null);
	}
	
	public MutableMultipleChoiceFieldEditor(IWidgetSettings settings) {
		super(settings);
		bar.setCommands(commands);
		bar.getLayout().direction = SWT.VERTICAL;
		bar.getLayout().setMargins(0, 0, 0, 0);
		bar.addCommandBarListener(this);
		bar.setWidgetSettings(settings);
	}

	public void setChange(SpecialWizard change) {
		this.change = change;
	}

	protected void adjustForNumColumns(int numColumns) {
		GridData gd = (GridData)getListControl().getLayoutData();
		gd.horizontalSpan = numColumns - 2;
		// We only grab excess space if we have to
		// If another field editor has more columns then
		// we assume it is setting the width.
		gd.grabExcessHorizontalSpace = gd.horizontalSpan == 1;
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		getLabelComposite(parent);
		Control control = getListChangeControl(parent);
		control.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
	}
	
	protected Composite getListChangeControl(Composite parent) {
		createListChangeControl(parent);
		return composite;
	}
	
	protected Control createListChangeControl(Composite parent) {
		GridData gd;
		Control control;
		if(composite == null)
			composite = new Composite(parent, SWT.NONE);
		composite.setBackground(parent.getBackground());
		GridLayout gridLayout = new GridLayout(3, false);
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.horizontalSpacing = 0;
		gridLayout.verticalSpacing = 0;
		composite.setLayout(gridLayout);
		
		Control listControl = createListControl(composite);
		gd = new GridData(GridData.FILL_BOTH);
		listControl.setLayoutData(gd);

		control = new Label(composite, SWT.NONE);
		control.setBackground(parent.getBackground());
		gd = new GridData();
		gd.widthHint = 5;
		control.setLayoutData(gd);
		
		control = getChangeControl(composite);

		return composite;
	}
	
	protected Control getChangeControl(Composite parent) {
		if(bar.getControl() != null && !bar.getControl().isDisposed()) {
			return bar.getControl();
		}
		return bar.createControl(parent);
	}

	public void action(String command) {
		if(NEW.equals(command)) {
			String newValue = changePressed();
			if (newValue != null) {
				resetChoices();
			}
		} else if(SELECT_ALL.equals(command)) {
			Choice[] cs= (Choice[])choicesArray.toArray(new Choice[0]);
			for (int i = 0; i < cs.length; i++) {
				cs[i].setSelected(true);
			}
			fireValueChange();			
		} else if(DESELECT_ALL.equals(command)) {
			Choice[] cs= (Choice[])choicesArray.toArray(new Choice[0]);
			for (int i = 0; i < cs.length; i++) {
				cs[i].setSelected(false);
			}
			fireValueChange();			
		}		
	}

	protected String changePressed() {
		if(change == null) return null;
		Properties p = new Properties();
		p.put("shell", bar.getControl().getShell()); //$NON-NLS-1$
		change.setObject(p);
		int i = change.execute();
		if(i != 0) return null;
		return p.getProperty("value"); //$NON-NLS-1$
	}

	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent), getListChangeControl(parent)};
	}

}
