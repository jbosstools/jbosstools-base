/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Exadel, Inc.
 *     Red Hat, Inc. 
 *******************************************************************************/
package org.jboss.tools.common.gef.xpl;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

import org.eclipse.swt.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.draw2d.ColorConstants;

public class GEFSplitter extends Composite {

	private static final int SASH_WIDTH = 5;
	private static final int FIXED_DRAG_MINIMUM = 62;
	private static final int OTHER_DRAG_MINIMUM = 0;
	private static final String MAINTAIN_SIZE = "maintain size";

	private int fixedSize = 150;
	private int fixedDragMinimum;
	private int otherDragMinimum;
	private int orientation = SWT.HORIZONTAL;
	private Sash[] sashes = new Sash[0];
	private Control[] controls = new Control[0];
	private Control maxControl = null;
	private Listener sashListener;
	private boolean first = true;

	protected PropertyChangeSupport listeners = new PropertyChangeSupport(this);

	public void addFixedSizeChangeListener(PropertyChangeListener listener) {
		listeners.addPropertyChangeListener(listener);
	}

	protected void firePropertyChange(int oldValue, int newValue) {
		listeners.firePropertyChange(MAINTAIN_SIZE, oldValue, newValue);
	}

	public void removeFixedSizeChangeListener(PropertyChangeListener listener) {
		listeners.removePropertyChangeListener(listener);
	}

	public int getFixedSize() {
		return fixedSize;
	}

	public void setFixedSize(int newSize) {
		if (newSize == fixedSize) {
			return;
		}
		firePropertyChange(fixedSize, fixedSize = newSize);
	}

	class SashPainter implements Listener {
		public void handleEvent(Event e) {
			paint((Sash)e.widget, e.gc);
		}
	}

	public GEFSplitter(Composite parent, int style) {
		this(parent, style, FIXED_DRAG_MINIMUM, OTHER_DRAG_MINIMUM);
	}

	public GEFSplitter(Composite parent, int style, int fixedDragMinimum) {
		this(parent, style, fixedDragMinimum, OTHER_DRAG_MINIMUM);
	}

	public GEFSplitter(Composite parent, int style, int fixedDragMinimum, int otherDragMinimum) {
		super(parent, checkStyle(style));
		this.fixedDragMinimum = fixedDragMinimum;
		this.otherDragMinimum = otherDragMinimum;

		if ((style & SWT.VERTICAL) != 0) {
			orientation = SWT.VERTICAL;
		}
	
		this.addListener(SWT.Resize, new Listener() {
			public void handleEvent(Event e) {
				layout(true);
			}
		});
	
		sashListener = new Listener() {
			public void handleEvent(Event e) {
				onDragSash(e);
			}
		};
	}

	private static int checkStyle (int style) {
		int mask = SWT.BORDER;
		return style & mask;
	}

	public Point computeSize (int wHint, int hHint, boolean changed) {
	
		Control[] controls = getControls(true);
		if (controls.length == 0) return new Point(wHint, hHint);
	
		int width = 0;
		int height = 0;
		boolean vertical = (getStyle() & SWT.VERTICAL) != 0;
		if (vertical) {
			width = wHint;
			height += (controls.length - 1) * SASH_WIDTH;
		} else {
			height = hHint;
			width += controls.length * SASH_WIDTH;
		}
		for (int i = 0; i < controls.length; i++) {
			if (vertical) {
				Point size = controls[i].computeSize(wHint, SWT.DEFAULT);
				height += size.y;	
			} else {
				Point size = controls[i].computeSize(SWT.DEFAULT, hHint);
				if (controls[i].getData(MAINTAIN_SIZE) != null) {
					size.x = fixedSize;
				}
				width += size.x;
			}
		}
		return new Point(width, height);
	}

	public int getOrientation() {
		return orientation;
	}

	public Control getMaximizedControl() {
		return this.maxControl;
	}

	private Control[] getControls(boolean onlyVisible) {
		Control[] children = getChildren();
		Control[] controls = new Control[0];
		for (int i = 0; i < children.length; i++) {
			if (children[i] instanceof Sash)
				continue;
			if (onlyVisible && !children[i].getVisible()) continue;

			Control[] newControls = new Control[controls.length + 1];
			System.arraycopy(controls, 0, newControls, 0, controls.length);
			newControls[controls.length] = children[i];
			controls = newControls;
		}
		return controls;
	}

	public void layout(boolean changed) {
		Rectangle area = getClientArea();
		if (area.width == 0 || area.height == 0) return;
	
		Control[] newControls = getControls(true);
		if (controls.length == 0 && newControls.length == 0) return;
		controls = newControls;
	
		if (maxControl != null && !maxControl.isDisposed()) {
			for (int i = 0; i < controls.length; i++) {
				if (controls[i] != maxControl) {
					controls[i].setBounds(-200, -200, 0, 0);
				} else {
					controls[i].setBounds(area);
				}
			}
			return;
		}
	
		if (sashes.length < controls.length - 1) {
			Sash[] newSashes = new Sash[controls.length - 1];
			System.arraycopy(sashes, 0, newSashes, 0, sashes.length);
			int sashOrientation =
				(orientation == SWT.HORIZONTAL) ? SWT.VERTICAL : SWT.HORIZONTAL;
			for (int i = sashes.length; i < newSashes.length; i++) {
				newSashes[i] = new Sash(this, sashOrientation);
				newSashes[i].setBackground(ColorConstants.button);
				newSashes[i].addListener(SWT.Paint, new SashPainter());
				newSashes[i].addListener(SWT.Selection, sashListener);
			}
			sashes = newSashes;
		}
		if (sashes.length > controls.length - 1) {
			if (controls.length == 0) {
				for (int i = 0; i < sashes.length; i++) {
					sashes[i].dispose();
				}
				sashes = new Sash[0];
			} else {
				Sash[] newSashes = new Sash[controls.length - 1];
				System.arraycopy(sashes, 0, newSashes, 0, newSashes.length);
				for (int i = controls.length - 1; i < sashes.length; i++) {
					sashes[i].dispose();
				}
				sashes = newSashes;
			}
		}
	
		if (controls.length == 0) return;
	
		if (orientation == SWT.HORIZONTAL) {
			int x = area.x;
			int width;
			for (int i = 0; i < controls.length; i++) {
				Control control = controls[i];
				if (control.getData(MAINTAIN_SIZE) != null) {
					width = fixedSize;
					if (width > area.width) {
						width = area.width - SASH_WIDTH;
					}
				} else {
					width = Math.max(area.width - fixedSize - SASH_WIDTH, 0);
				}
				control.setBounds(x, area.y, width, area.height);
				x += (width + SASH_WIDTH);
			}
			sashes[0].setBounds(controls[0].getBounds().x + controls[0].getBounds().width, area.y, 
								SASH_WIDTH, area.height);
		} else {
			int y = area.y;
			int height;
			for (int i = 0; i < controls.length; i++) {
				Control control = controls[i];
				if (control.getData(MAINTAIN_SIZE) != null) {
					height = fixedSize;
					if (height > area.height) {
						height = area.height - SASH_WIDTH;
					}
				} else {
					height = Math.max(area.height - fixedSize - SASH_WIDTH, 0);
				}
				control.setBounds(area.x, y, area.width, height);
				y += (height + SASH_WIDTH);
			}
			sashes[0].setBounds(area.x, controls[0].getBounds().y + controls[0].getBounds().height,
								area.width, SASH_WIDTH);
		}
	}

	public void maintainSize(Control c) {
		Control[] controls = getControls(false);
		for (int i = 0; i < controls.length; i++) {
			Control ctrl = controls[i];
			if (ctrl == c) {
				ctrl.setData(MAINTAIN_SIZE, new Boolean(true));
			}
		}
	}


	void paint(Sash sash, GC gc) {
		if (first) {
			first = false;
			getParent().layout();
		}
		Point size = sash.getSize();
		if (getOrientation() == SWT.HORIZONTAL) {
			gc.setForeground(ColorConstants.buttonDarker);
			gc.drawLine(SASH_WIDTH - 1, 0, SASH_WIDTH - 1, size.y);
			gc.setForeground(ColorConstants.buttonLightest);
			gc.drawLine(0, 0, 0, size.y);
		} else {
			gc.setForeground(ColorConstants.buttonDarker);
			gc.drawLine(0, SASH_WIDTH - 1, size.x, SASH_WIDTH - 1);
			gc.setForeground(ColorConstants.buttonLightest);
			gc.drawLine(0, 0, size.x, 0);
		}
	}

	private void onDragSash(Event event) {
		if (event.detail == SWT.DRAG) {
			Rectangle area = getClientArea();
			if (orientation == SWT.HORIZONTAL) {
				if (controls[0].getData(MAINTAIN_SIZE) != null) {
					event.x = Math.max(Math.min(area.width - otherDragMinimum - SASH_WIDTH, event.x), fixedDragMinimum); 
				} else {
					event.x = Math.min(Math.max(otherDragMinimum, event.x), area.width - fixedDragMinimum - SASH_WIDTH); 
				}
			} else {
				if (controls[0].getData(MAINTAIN_SIZE) != null) {
					event.x = Math.max(Math.min(area.height - otherDragMinimum - SASH_WIDTH, event.y), fixedDragMinimum); 
				} else {
					event.x = Math.min(Math.max(otherDragMinimum, event.y), area.height - fixedDragMinimum - SASH_WIDTH); 
				}
			}
			return;
		}

		Sash sash = (Sash)event.widget;
		int sashIndex = -1;
		for (int i = 0; i < sashes.length; i++) {
			if (sashes[i] == sash) {
				sashIndex = i;
				break;
			}
		}
		if (sashIndex == -1) return;

		Control c1 = controls[sashIndex];
		Control c2 = controls[sashIndex + 1];
		Rectangle b1 = c1.getBounds();
		Rectangle b2 = c2.getBounds();
	
		Rectangle sashBounds = sash.getBounds();
		if (orientation == SWT.HORIZONTAL) {
			int shift = event.x - sashBounds.x;
			if (shift == 0) {
				return;
			}
			b1.width += shift;
			b2.x += shift;
			b2.width -= shift;
		} else {
			int shift = event.y - sashBounds.y;
			if (shift == 0) {
				return;
			}
			b1.height += shift;
			b2.y += shift;
			b2.height -= shift;
		}
	
		c1.setBounds(b1);
		sash.setBounds(event.x, event.y, event.width, event.height);
		c2.setBounds(b2);

		if (orientation == SWT.HORIZONTAL) {
			if (c1.getData(MAINTAIN_SIZE) != null) {
				setFixedSize(c1.getBounds().width);
			} else {
				setFixedSize(c2.getBounds().width);
			}
		} else {
			if (c1.getData(MAINTAIN_SIZE) != null) {
				setFixedSize(c1.getBounds().height);
			} else {
				setFixedSize(c2.getBounds().height);
			}
		}
	}

	public void setOrientation(int orientation) {
		if (this.orientation == orientation) return;
		if (orientation != SWT.HORIZONTAL && orientation != SWT.VERTICAL) {
			SWT.error(SWT.ERROR_INVALID_ARGUMENT);
		}
		this.orientation = orientation;
	
		int sashOrientation = (orientation == SWT.HORIZONTAL) ? SWT.VERTICAL : SWT.HORIZONTAL;
		for (int i = 0; i < sashes.length; i++) {
			sashes[i].dispose();
			sashes[i] = new Sash(this, sashOrientation);
			sashes[i].setBackground(ColorConstants.buttonLightest);
			sashes[i].addListener(SWT.Selection, sashListener);
		}
		layout();
	}

	public void setLayout (Layout layout) {
	}

	public void setMaximizedControl(Control control) {
		if (control == null) {
			if (maxControl != null) {
				this.maxControl = null;
				layout();
				for (int i = 0; i < sashes.length; i++) {
					sashes[i].setVisible(true);
				}
			}
			return;
		}
	
		for (int i = 0; i < sashes.length; i++) {
			sashes[i].setVisible(false);
		}
		maxControl = control;
		layout();
	}
}
