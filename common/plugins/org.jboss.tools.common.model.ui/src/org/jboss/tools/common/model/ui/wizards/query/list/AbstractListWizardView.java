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
package org.jboss.tools.common.model.ui.wizards.query.list;

import java.util.Properties;
import org.jboss.tools.common.model.ui.action.CommandBar;
import org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizardView;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

public abstract class AbstractListWizardView extends AbstractQueryWizardView {
	protected CommandBar allBar = new CommandBar();
	protected Button[] boxes = new Button[0];
	protected String[][] vs = new String[0][];
	
	public AbstractListWizardView() {
		createAllBar();
	}

	public void dispose() {
		super.dispose();
		if (allBar!=null) allBar.dispose();
		allBar = null;
	}
	
	protected abstract String[] getActions();
	
	protected void createAllBar() {
		allBar.getLayout().direction = SWT.VERTICAL;
		allBar.setCommands(getActions());
		allBar.addCommandBarListener(this);
	}
	
	public void setObject(Object data) {
		super.setObject(data);
		Properties p = (Properties)data;
		vs = (String[][])p.get("data"); //$NON-NLS-1$
		boxes = new Button[vs.length];
	}
	
	public Control createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(2, false);
		layout.horizontalSpacing = 10;
		layout.marginHeight = 10;
		layout.verticalSpacing = 10;
		layout.marginWidth = 10;
		composite.setLayout(layout);
		GridData gd = new GridData(GridData.FILL_BOTH);
		composite.setLayoutData(gd);
		
		ScrolledComposite sp = new ScrolledComposite(composite, SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
		
		sp.setLayout(new GridLayout());
		Composite c = new Composite(sp, SWT.NONE);
		c.setLayoutData(new GridData(GridData.FILL_BOTH));
		sp.setContent(c);
		c.setLayout(new GridLayout());
		for (int i = 0; i < vs.length; i++) {
			boxes[i] = new Button(c, SWT.CHECK);
			boxes[i].setText(vs[i][0]);
			boxes[i].setSelection(!"yes".equals(vs[i][1])); //$NON-NLS-1$
			boxes[i].addSelectionListener(new AL(i));
		}
		c.pack();
		c.layout();
		Control bc = allBar.createControl(composite);
		bc.setLayoutData(new GridData(GridData.FILL_VERTICAL));
		return sp;
	}
	
	public void action(String command) {
		if(CANCEL.equals(command) ||
		   OK.equals(command) ||
		   HELP.equals(command)) {
		   	super.action(command);
		} else { 
			stopEditing();
			internalAction(command);
		}
	}

	protected abstract void internalAction(String command);
	
	class AL extends SelectionAdapter {
		int i;
		public AL(int i) {
			this.i = i;
		}
		public void widgetSelected(SelectionEvent e) {
			apply(i);
		}
	}
	
	protected void apply(int i) {
		vs[i][1] = (boxes[i].getSelection()) ? "no" : "yes"; //$NON-NLS-1$ //$NON-NLS-2$
	}

}
