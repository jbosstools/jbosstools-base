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
package org.jboss.tools.common.model.ui.widgets;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.ScrollBar;

/**
 * @author AU
 */
public class ScrolledComposite extends Composite {
	
	private Composite viewport;
	private boolean fixedWidth;
	private boolean fixedHeight;
	private boolean inResize;
	private boolean grabHSpace = Boolean.TRUE.booleanValue();
	private boolean grabVSpace = Boolean.TRUE.booleanValue();

	public ScrolledComposite(Composite parent, int style) {
		super(parent, checkStyle(style));
		setBackground(parent.getBackground());
	
		ScrollBar hBar = getHorizontalBar ();
		if (hBar != null) {
			hBar.addListener (SWT.Selection, new Listener () {
				public void handleEvent (Event e) {
					hScroll();
				}
			});
		} else {
			this.fixedWidth = Boolean.TRUE.booleanValue();
		}
	
		ScrollBar vBar = getVerticalBar ();
		if (vBar != null) {
			vBar.addListener (SWT.Selection, new Listener () {
				public void handleEvent (Event e) {
					vScroll();
				}
			});
		} else {
			this.fixedHeight = Boolean.TRUE.booleanValue();
		}
	
		addListener (SWT.Resize,  new Listener () {
			public void handleEvent (Event e) {
				resize();
			}
		});
		
		Label label = new Label(this, SWT.NONE);
		label.setText("Test");
		
		viewport = new Viewport(this, SWT.NONE);
	}

	private static int checkStyle (int style) {
		int mask = SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.LEFT_TO_RIGHT | SWT.RIGHT_TO_LEFT;
		return style & mask;
	}
	
	private void vScroll() {
		if (viewport == null) return;
		Point location = viewport.getLocation ();
		ScrollBar vBar = getVerticalBar ();
		int vSelection = vBar.getSelection ();
		viewport.setLocation (location.x, -vSelection);
	}

	private void hScroll() {
		if (viewport == null) return;
		Point location = viewport.getLocation ();
		ScrollBar hBar = getHorizontalBar ();
		int hSelection = hBar.getSelection ();
		viewport.setLocation (-hSelection, location.y);
	}

	private void resize() {
		if (inResize) return;
		inResize = Boolean.TRUE.booleanValue();
		layout();
		inResize = Boolean.FALSE.booleanValue();
	}
	
	public Composite getViewport() {
		return viewport;
	}

	public void setLayout(Layout layout) {
		if (viewport!=null) viewport.setLayout(layout);
	}

	private int lock = 0;
	public void layout(boolean changed) {
		checkWidget();
		if (viewport == null) return;
		if(lock > 0) return; //EFWPE-617
		++lock;
		try {
			doLayout();
		} finally {
			--lock;
		}
	}
	
	private void doLayout() {
		Point viewportSize = viewport.computeSize(SWT.DEFAULT, SWT.DEFAULT, Boolean.TRUE.booleanValue());
		Point viewportLocation = viewport.getLocation();
		Rectangle cr = new Rectangle(viewportLocation.x, viewportLocation.y, viewportSize.x, viewportSize.y);
		Rectangle hr = getClientArea();
		if (fixedWidth) {
			cr.width = hr.width;	
		} else if (grabHSpace) {
			cr.width = Math.max(cr.width, hr.width);	
		}
		if (fixedHeight) {
			cr.height = hr.height;	
		} else if (grabVSpace) {
			cr.height = Math.max(cr.height, hr.height);
		}
		layoutHBar(cr, hr);
		layoutVBar(cr, hr);
		viewport.setBounds(cr);
	}
	
	private void layoutHBar(Rectangle cr, Rectangle hr) {
		ScrollBar hBar = getHorizontalBar ();
		if(hBar == null) return;
		hBar.setMaximum (cr.width);
		hBar.setThumb(Math.min (cr.width, hr.width));
		int hs = hBar.getSelection ();
		if (hs >= cr.width - hr.width) {
			if (cr.width <= hr.width) {
				hs = 0;
				hBar.setSelection(0);
			}
			cr.x = -hs;
		}
	}
	private void layoutVBar(Rectangle cr, Rectangle hr) {
		ScrollBar vBar = getVerticalBar();
		if(vBar == null) return;
		vBar.setMaximum(cr.height);
		vBar.setThumb(Math.min (cr.height, hr.height));
		int vs = vBar.getSelection ();
		if (vs >= cr.height - hr.height) {
			if (cr.height <= hr.height) {
				vs = 0;
				vBar.setSelection(0);
			}
			cr.y = -vs;
		}
	}

	public Point computeSize (int wHint, int hHint, boolean changed) {
		checkWidget ();
		if (viewport == null) {
			return super.computeSize (wHint, hHint, changed);
		}
		if (wHint>16) wHint = wHint-16;
		Point size = viewport.computeSize (wHint, hHint, changed);
		Rectangle trim = computeTrim (0, 0, size.x, size.y);
		//ModelUIPlugin.log("+++ computeSize="+trim);
		return new Point (trim.width, trim.height);
	}
	
	public static void scrollToVisible(Control control, Rectangle r) {
		int y = r == null ? 0 : r.y;
		Rectangle pr = control.getBounds();
		y += pr.y;
		Composite parent = control.getParent();
		while(parent != null && !(parent instanceof Viewport)) {
			pr = parent.getBounds();
			y += pr.y;
			parent = parent.getParent();
		}
		if(parent == null) return;
		Viewport v = (Viewport)parent;
		pr = v.getBounds();
		ScrolledComposite sc = (ScrolledComposite)v.getParent();
		int availableH = sc.getBounds().height;
		int viewportH = pr.height;
		if(viewportH <= availableH) return;
		ScrollBar bar = sc.getVerticalBar();
		int dh = r == null ? 10 : r.height;
		int up = dh + 2 - y;
		int down = availableH - dh - 2 - dh - y;
		if(up > down) {
			up = (up + down) / 2 - 5;
			down = up + 10;
		}
		int delta = 0;
		if(pr.y > down) {
			delta = down - pr.y;
		} else if(pr.y < up) {
			delta = up - pr.y;
		}
		int sDelta = (bar.getMaximum() - bar.getMinimum() - bar.getThumb()) * (-delta) / (viewportH - availableH);
		bar.setSelection(bar.getSelection() +  sDelta);
		pr.y += delta;
		v.setBounds(pr);
		v.layout(true);
	}
}
