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

import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.ui.forms.FormColors;
import org.eclipse.ui.forms.widgets.FormToolkit;

import org.jboss.tools.common.model.ui.ModelUIImages;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.widgets.WhiteSettings;

public class DefaultFormPage implements IFormPage, PaintListener {

	public static final String IMG_FORM_BANNER = "form_banner.gif";
	
	private ISelectionProvider selectionProvider;

	protected IWidgetSettings settings;

	private int HEADER_H_GAP = 10;
	private int HEADER_V_GAP = 5;

	protected Color headingBackground;
	protected Color headingForeground = Display.getDefault().getSystemColor(SWT.COLOR_BLUE);

	protected IForm form;
	protected Control control;
	protected Image image;
	
	protected Font titleFont;
	
	protected String label;
	protected String title;
	
	public DefaultFormPage() {
		image = ModelUIImages.getImage(IMG_FORM_BANNER);
	}

	public DefaultFormPage(IForm form) {
		this.form = form;
		image = ModelUIImages.getImage(IMG_FORM_BANNER);
	}

	public String getLabel() {
		if (label!=null) return label;
		if (form!=null) return form.getHeadingText();
		return  "Null";
	}

	public String getTitle() {
		if (title!=null) return title;
		if (form!=null) return form.getHeadingText();
		return  "Null";
	}


	public boolean becomesInvisible(IFormPage newPage) {
		return true;
	}

	public void becomesVisible(IFormPage previousPage) {
	}

	public Control createFormControl(Composite parent, IWidgetSettings settings) {
		Control control;
		if (form==null) {
			control = new Composite(parent, SWT.NONE);
			settings.setupControl(control);
		} else {
			control = form.createControl(parent, settings); 
		}
		return control;
	}

	public Control createControl(Composite parent) {
		settings = new WhiteSettings();
		titleFont = settings.getFont("Label.Font.Title");
		Composite composite = new Composite(parent, SWT.NONE);
		settings.setupControl(composite);
		composite.addPaintListener(this);
		composite.setLayout(new FormLayout());
		composite.setMenu(parent.getMenu());
		
		Control formControl = createFormControl(composite, settings); 
		GridData gd = new GridData(GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL);
		formControl.setLayoutData(gd);
		
		control = composite;
		return control;
	}

	public Control getControl() {
		return control;
	}

	public boolean isSource() {
		return false;
	}
	
	public boolean isVisible() {
		return false;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public void setTitle(String title) {
		this.title = title;
	}
	
	private int getTitleHeight() {
		int imageHeight = 0;
		if (image != null
			&& SWT.getPlatform().equals("motif") == false) {
			imageHeight = image.getBounds().height;
		}
		int height = getFontHeight() + 2 * HEADER_V_GAP;
		return Math.max(height, imageHeight);
	}
	
	private int getFontHeight() {
		GC gc = new GC(control);
		FontMetrics fm = gc.getFontMetrics();
		int fontHeight = fm.getHeight();
		gc.dispose();
		return fontHeight;
	}
	
	
	private void paint(Control form, GC gc) {
		if (image != null) {
			paintWithImage(form, gc);
		} else {
			gc.setBackground(settings.getColor("Composite.Background"));
			gc.setForeground(settings.getColor("Composite.Foreground"));
		}
		gc.setFont(titleFont);
		gc.drawText(getTitle(), HEADER_H_GAP, HEADER_V_GAP, true);
	}
	
	private void paintWithImage(Control form, GC gc) {
		Rectangle bounds = form.getBounds();
		if (headingBackground != null) {
			gc.setBackground(headingBackground);
			gc.fillRectangle(0, 0, bounds.width, getTitleHeight());
		}
		if (!SWT.getPlatform().equals("motif")) {
//			gc.drawImage(image, 0, 0);
		}
		Color fg = (headingForeground != null) ? headingForeground : settings.getColor("Composite.Foreground");
		FormToolkit t = settings.getToolkit(form.getDisplay());
		if(t != null && t.getColors().getColor(FormColors.TITLE) != null) {
			fg = t.getColors().getColor(FormColors.TITLE);
		}
		gc.setForeground(fg);
	}

	public final void paintControl(PaintEvent event) {
		paint((Control) event.widget, event.gc);
	}

	private int widthHint = SWT.DEFAULT;
	private int heightHint = SWT.DEFAULT;

	class FormLayout extends Layout {
		protected Point computeSize(Composite composite, int wHint, int hHint, boolean flushCache) {
			if (wHint != SWT.DEFAULT && hHint != SWT.DEFAULT)
				return new Point(wHint, hHint);
			Control client = composite.getChildren()[0];
			Point csize = client.computeSize(widthHint, heightHint, flushCache);
			csize.y += getTitleHeight();
			return csize;
		}
		protected void layout(Composite composite, boolean flushCache) {
			Rectangle r = composite.getClientArea();
			int th = getTitleHeight();
			composite.getChildren()[0].setBounds(r.x, r.y + th, r.width, r.height - th);
		}
	}

	public void initialize(Object model) {
	}

	public void commitChanges(boolean onSave) {
	}

	public void expandTo(Object object) {
	}

	public void update() {
	}
	
	public ISelectionProvider getSelectionProvider() {
		return selectionProvider;
	}

	public void setSelectionProvider(ISelectionProvider provider) {
		selectionProvider = provider;
	}

	public void dispose() {
		if (this.control!=null) this.control.dispose();
		this.control = null;
	}

}
