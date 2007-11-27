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
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.ScrollBar;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.widgets.ScrolledComposite;

public class ScrollableFormContainer extends DefaultFormContainer {
	public static final int H_SCROLL_INCREMENT = 5;
	public static final int V_SCROLL_INCREMENT = 64;

	private IWidgetSettings settings;
	private ScrolledComposite scrolledComposite;
	private boolean verticalFit = false;
	private boolean scrollable = true;
	
	public ScrollableFormContainer() {
	}

	public ScrollableFormContainer(IForm form) {
		super(form);
	}
	
	public void dispose() {
		super.dispose();
		if (scrolledComposite!=null && !scrolledComposite.isDisposed()) scrolledComposite.dispose();
		scrolledComposite = null;
	}

	public Control createControl(Composite parent, IWidgetSettings settings) {
		this.settings = settings;
		scrolledComposite = new ScrolledComposite(parent, SWT.V_SCROLL);
		GridLayout layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		layout.verticalSpacing = 0;
		layout.horizontalSpacing = 0;
		scrolledComposite.setLayout(layout);
		if (getLayoutData()!=null) scrolledComposite.setLayoutData(getLayoutData());
		return scrolledComposite;
	}
	
	public Control getControl() {
		return scrolledComposite;
	}

	public boolean isScrollable() {
		return scrollable;
	}

	public void setScrollable(boolean b) {
		scrollable = b;
	}

	public boolean isVerticalFit() {
		return verticalFit;
	}

	public void setVerticalFit(boolean b) {
		verticalFit = b;
	}

/*
	private void initializeScrollBars(ScrolledComposite scomp) {
		ScrollBar hbar = scomp.getHorizontalBar();
		if (hbar != null) {
			hbar.setIncrement(H_SCROLL_INCREMENT);
		}
		ScrollBar vbar = scomp.getVerticalBar();
		if (vbar != null) {
			vbar.setIncrement(V_SCROLL_INCREMENT);
		}
		updatePageIncrement(scomp);
	}
*/

	public void update() {
		super.update();
		if (scrolledComposite instanceof ScrolledComposite) {
			updateScrolledComposite();
		} else if(scrolledComposite!=null) {
			scrolledComposite.layout(true);
		}
	}
	
	public void updateScrollBars() {
		if (scrolledComposite instanceof ScrolledComposite) {
			updateScrolledComposite();
		}
	}
	public void updateScrolledComposite() {
		ScrolledComposite sc = (ScrolledComposite)scrolledComposite;
		getControl();
		if ((scrolledComposite.getViewport()!=null)&&(!scrolledComposite.getViewport().isDisposed())) {
			//Point newSize = formControl.computeSize(sc.getClientArea().width, sc.getClientArea().height);
			Point oldSize = scrolledComposite.getViewport().getSize();
			Point newSize = scrolledComposite.getViewport().computeSize(SWT.DEFAULT, SWT.DEFAULT);
			if (oldSize!=null && oldSize.equals(newSize)) return;
			if (newSize!=null && oldSize!=null && oldSize.x>=newSize.x && oldSize.y>=newSize.y) return;
			scrolledComposite.getViewport().setSize(newSize);
			//sc.setMinSize(newSize);
			updatePageIncrement(sc);
		}
	}

	public static void updatePageIncrement(ScrolledComposite scomp) {
		ScrollBar vbar = scomp.getVerticalBar();
		if (vbar != null) {
			Rectangle clientArea = scomp.getClientArea();
			int increment = clientArea.height - 5;
			vbar.setPageIncrement(increment);
		}
	}


	public boolean addForm(IForm form) {
		if (scrolledComposite != null) {
			Control control = form.getControl();
			if (control == null) {
				control = form.createControl(this.scrolledComposite.getViewport(), this.settings);
			} else {
				control.setParent(this.scrolledComposite.getViewport());
			}
			GridData gd;
			gd = new GridData(GridData.FILL_BOTH);
			control.setLayoutData(gd);
			reflow();
		}
		form.setParent(this);
		form.setEnabled(isEnabled());
		return forms.add(form);
	}

	protected void reflow() {
		this.scrolledComposite.getViewport().setRedraw(false);
		this.scrolledComposite.getViewport().getParent().setRedraw(false);
		this.scrolledComposite.getViewport().layout(true);
		this.scrolledComposite.getViewport().getParent().layout(true);
		this.scrolledComposite.getViewport().setRedraw(true);
		this.scrolledComposite.getViewport().getParent().setRedraw(true);
	}
	
	
/*
	public boolean addForm(IForm form) {
		boolean b = super.addForm(form);
		update();
		return b;
	}
*/	
}