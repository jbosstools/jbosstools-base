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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.forms.events.ExpansionEvent;
import org.eclipse.ui.forms.events.IExpansionListener;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class ExpandableForm extends DefaultForm {
	
	public static final int SELECTION = 1;
	private Control client;
	//private SectionChangeManager sectionManager;
	private String description;
	protected Label descriptionLabel;
	private boolean descriptionPainted = true;
	private boolean collapsable = true;
	private boolean collapsed = false;
	private Composite control;
	private static final String COLLAPSED_ID = "collapsed";
	
	protected Section section;
	
	private Layout layout;

	public ExpandableForm() {
	}

	public void dispose() {
		super.dispose();
		if (descriptionLabel!=null && descriptionLabel.isDisposed()) descriptionLabel.dispose();
		descriptionLabel = null;
		if (client!=null && !client.isDisposed()) client.dispose();
		client = null;
		if (control!=null && !control.isDisposed()) control.dispose();
		control = null;
		if(section != null) {
			if(!section.isDisposed()) section.dispose();
			section = null;
		}
	}

	public Control getControl() {
		return control;
	}

	public Control createControl(Composite parent, IWidgetSettings settings) {
		FormToolkit toolkit = settings.getToolkit(parent.getDisplay());
		if(toolkit == null) {
			toolkit = new FormToolkit(parent.getDisplay());
		}

		section = toolkit.createSection(parent, ExpandableComposite.TWISTIE|ExpandableComposite.EXPANDED|ExpandableComposite.TITLE_BAR);
		section.setText("" + getHeadingText());
		section.setLayoutData(new GridData(GridData.FILL_HORIZONTAL | /*GridData.FILL_VERTICAL |*/ GridData.VERTICAL_ALIGN_BEGINNING));
		toolkit.adapt(section);
		section.setBackground(parent.getBackground());
		
		Composite composite = section;
//		settings.setupControl(composite);
		composite.setLayout(new GridLayout(3, false)); // self layout!
		composite.setLayoutData(getLayoutData());
		composite.setData(this);

		if (collapsable) {
			section.addExpansionListener(new IExpansionListener() {
				public void expansionStateChanged(ExpansionEvent e) {
					collapsed = !section.isExpanded();
				}
				public void expansionStateChanging(ExpansionEvent e) {
				}
			});
		}

		if (descriptionPainted && description != null) {
			descriptionLabel = new Label(composite, SWT.NONE | SWT.WRAP);
			settings.setupControl(descriptionLabel);
		}

		client = createClientArea(composite, settings);
		
		composite.setData(this);
		
		control = composite;

		if (this.isCollapsed()) {
			section.setExpanded(false);
		}
		
//		section.setClient(composite);
		section.setClient(client);

		return control;
	}

	protected void reflow() {
		if (control==null) return;
		Composite parent = control.getParent();
		Control parentParent = null;
		if (parent!=null) parentParent = parent.getParent();
		
		control.setRedraw(false);
		if (parent!=null) control.getParent().setRedraw(false);
		if (parentParent!=null) control.getParent().getParent().setRedraw(false);
		control.layout(true);
		if (parent!=null) control.getParent().layout(true);
		if (parentParent!=null) control.getParent().getParent().layout(true);
		control.setRedraw(true);
		if (parent!=null) control.getParent().setRedraw(true);
		if (parentParent!=null) control.getParent().getParent().setRedraw(true);
	}

	public Layout getLayout() {
		return layout;
	}

	public void setLayout(Layout layout) {
		this.layout = layout;
	}

	public boolean isCollapsable() {
		return collapsable;
	}

	public void setCollapsable(boolean b) {
		collapsable = b;
	}

    public boolean isCollapsed() {
        return collapsed;
    }

    public void setCollapsed(boolean b) {
        collapsed = b;
    }
    
	public void load(IMemento memento) {
		String collapsed = memento.getString(COLLAPSED_ID);
		if (collapsed!=null) setCollapsed(Boolean.valueOf(collapsed).booleanValue());
	}

	public void store(IMemento memento) {
		memento.putString(COLLAPSED_ID, ""+isCollapsed());
	}

}
