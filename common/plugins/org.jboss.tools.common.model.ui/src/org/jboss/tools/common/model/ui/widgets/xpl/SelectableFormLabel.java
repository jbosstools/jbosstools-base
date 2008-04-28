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
package org.jboss.tools.common.model.ui.widgets.xpl;

import org.eclipse.swt.SWT;
import org.eclipse.swt.accessibility.ACC;
import org.eclipse.swt.accessibility.Accessible;
import org.eclipse.swt.accessibility.AccessibleAdapter;
import org.eclipse.swt.accessibility.AccessibleControlAdapter;
import org.eclipse.swt.accessibility.AccessibleControlEvent;
import org.eclipse.swt.accessibility.AccessibleEvent;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.TypedListener;

public class SelectableFormLabel extends FormLabel {
	private boolean hasFocus;
	private Color passiveColor;
	private Color activeColor;
	private Color disabledColor;
	private Cursor activeCursor;
	boolean hover=false; //WARNING: never used
	private boolean oldUnderlined;

	public boolean getSelection() {
		return hasFocus;
	}

	//public boolean setFocus () {
	//	hasFocus = Boolean.TRUE.booleanValue();
	//	return hasFocus;
	//}
	
	/**
	 * Constructor for SelectableFormLabel
	 */
	public SelectableFormLabel(Composite parent, int style) {
		super(parent, style);
		setUnderlined(Boolean.TRUE.booleanValue());
		addKeyListener(new KeyAdapter() {
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					// Activation
					notifyListeners(SWT.DefaultSelection);
				}
			}
		});
		addListener(SWT.Traverse, new Listener () {
			public void handleEvent(Event e) {
				switch (e.detail) {
					case SWT.TRAVERSE_PAGE_NEXT:
					case SWT.TRAVERSE_PAGE_PREVIOUS:
					case SWT.TRAVERSE_ARROW_NEXT:
					case SWT.TRAVERSE_ARROW_PREVIOUS:
					case SWT.TRAVERSE_RETURN:
					e.doit = false;
					return;
				}
				e.doit = true;
			}
		});
		addMouseListener(new MouseListener(){
			public void mouseDoubleClick(MouseEvent e) {
			}

			public void mouseDown(MouseEvent e) {
				if (!hasFocus) {
				   hasFocus=true;
				   //notifyListeners(SWT.Selection);
				   redraw();
				}
			}

			public void mouseUp(MouseEvent e) {
				if (hasFocus) {
					hasFocus=false;
					notifyListeners(SWT.Selection);
					redraw();
				}
			}
		});
		addFocusListener(new FocusListener() {
			public void focusGained(FocusEvent e) {
				if (!hasFocus) {
				   hasFocus=true;
				   notifyListeners(SWT.Selection);
				   redraw();
				}
			}
			public void focusLost(FocusEvent e) {
				if (hasFocus) {
					hasFocus=false;
					notifyListeners(SWT.Selection);
					redraw();
				}
			}
		});
		
		addMouseTrackListener(new MouseTrackAdapter() {
			public void mouseEnter(MouseEvent e) {
				hover = true;
				setForeground(activeColor);
				if (activeCursor!=null)
				   setCursor(activeCursor);
				redraw();
			}
			public void mouseExit(MouseEvent e) {
				hover = false;
				setForeground(passiveColor);
				if (activeCursor!=null)
				   setCursor(null);
				redraw();
			}
		});
		
		marginWidth = 1;
		marginHeight = 1;
	}
	
	protected void initAccessible() {
		Accessible accessible = getAccessible();
		accessible.addAccessibleListener(new AccessibleAdapter() {
			public void getName(AccessibleEvent e) {
				e.result = getText();
			}

			public void getHelp(AccessibleEvent e) {
				e.result = getToolTipText();
			}
		});

		accessible
			.addAccessibleControlListener(new AccessibleControlAdapter() {
			public void getChildAtPoint(AccessibleControlEvent e) {
				Point pt = toControl(new Point(e.x, e.y));
				e.childID =
					(getBounds().contains(pt))
						? ACC.CHILDID_SELF
						: ACC.CHILDID_NONE;
			}

			public void getLocation(AccessibleControlEvent e) {
				Rectangle location = getBounds();
				Point pt = toDisplay(new Point(location.x, location.y));
				e.x = pt.x;
				e.y = pt.y;
				e.width = location.width;
				e.height = location.height;
			}

			public void getChildCount(AccessibleControlEvent e) {
				e.detail = 0;
			}

			public void getRole(AccessibleControlEvent e) {
				e.detail = ACC.ROLE_PUSHBUTTON;
			}

			public void getState(AccessibleControlEvent e) {
				e.detail = SelectableFormLabel.this.getSelection()?ACC.STATE_SELECTED:ACC.STATE_NORMAL;
			}
		});
	}
	
	private void notifyListeners(int eventType) {
		Event event = new Event();
		event.type = eventType;
		event.widget = this;
		notifyListeners(eventType, event);
	}

	protected void paint(PaintEvent e) {
		super.paint(e);
	   	if (hasFocus) {
	   		GC gc = e.gc;
	   		Point size = getSize();
	   		gc.setForeground(getForeground());
	   		gc.drawFocus(0, 0, size.x, size.y);
		}
	}

	public void addSelectionListener(SelectionListener listener) {
		checkWidget ();
		if (listener == null) return;
		TypedListener typedListener = new TypedListener (listener);
		addListener (SWT.Selection,typedListener);
		addListener (SWT.DefaultSelection,typedListener);
	}
	
	public void removeSelectionListener(SelectionListener listener) {
		checkWidget ();
		if (listener == null) return;
		removeListener (SWT.Selection, listener);
		removeListener (SWT.DefaultSelection, listener);
	}
	
	public void setActiveCursor(Cursor cursor) {
		activeCursor = cursor;
	}

	public void setActiveColor(Color color) {
		activeColor = color;
	}

	public void setPassiveColor(Color color) {
		passiveColor = color;
		// by default
		setForeground(color);
	}

	public Color getDisabledColor() {
		return disabledColor;
	}

	public void setDisabledColor(Color color) {
		disabledColor = color;
	}
	
	public void setEnabled(boolean enabled) {
		if (isEnabled()) {
			oldUnderlined = isUnderlined();
		}
		super.setEnabled(enabled);
		if (enabled) {
			this.setForeground(this.passiveColor);
			this.setUnderlined(oldUnderlined);
			this.setCursor(this.activeCursor);
		} else {
			if (disabledColor!=null) 
				this.setForeground(disabledColor); 
			else 
				this.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_WIDGET_NORMAL_SHADOW));
			this.setUnderlined(Boolean.FALSE.booleanValue());
			this.setCursor(null);
		}
		this.redraw();
	}
/*
	private boolean active;
	
	public boolean isActive() {
		return active;
	}

	public void setActive(boolean b) {
		active = b;
	}
*/
}