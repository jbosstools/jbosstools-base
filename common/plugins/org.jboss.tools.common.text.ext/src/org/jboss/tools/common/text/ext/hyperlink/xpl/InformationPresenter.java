/*******************************************************************************
 * Copyright (c) 2000, 2011 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Red Hat, Inc.
 *******************************************************************************/
package org.jboss.tools.common.text.ext.hyperlink.xpl;

import org.eclipse.jface.text.AbstractInformationControlManager;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.IViewportListener;
import org.eclipse.jface.util.Geometry;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;

public class InformationPresenter extends AbstractInformationControlManager{
	private ITextViewer viwer;

	public InformationPresenter(ITextViewer viwer, IInformationControlCreator creator) {
		super(creator);
		this.viwer = viwer;
		setCloser(new Closer());
	}

	@Override
	public void showInformation() {
		showInformation_internal(false);
	}
	
	private IInformationControl showInformation_internal(boolean test){
		IInformationControl iControl = getInformationControl();
		Point sizeConstraints= computeSizeConstraints(viwer.getTextWidget(), null, iControl);
		iControl.setSizeConstraints(sizeConstraints.x, sizeConstraints.y);
		Point size= null;
		Rectangle bounds= restoreInformationControlBounds();

		if (bounds != null) {
			if (bounds.width > -1 && bounds.height > -1)
				size= Geometry.getSize(bounds);
		}

		if (size == null)
			size= iControl.computeSizeHint();

		size= Geometry.max(size, sizeConstraints);

		iControl.setSize(size.x, size.y);
		if(test){
			((HierarchyInformationControl)iControl).setBlockOnOpen(false);
		}
		iControl.setVisible(true);
		return iControl;
	}
	
	public IInformationControl showInformationForTest(){
		return showInformation_internal(true);
	}

	@Override
	protected void computeInformation() {
		setInformation("Information", new Rectangle(1,1,100,100));
	}
	
	class Closer implements IInformationControlCloser, ControlListener, MouseListener, FocusListener, IViewportListener, KeyListener {

		/** The subject control. */
		private Control fSubjectControl;
		/** The information control. */
		private IInformationControl fInformationControlToClose;
		/** Indicates whether this closer is active. */
		private boolean fIsActive= false;

		/*
		 * @see IInformationControlCloser#setSubjectControl(Control)
		 */
		public void setSubjectControl(Control control) {
			fSubjectControl= control;
		}

		/*
		 * @see IInformationControlCloser#setInformationControl(IInformationControl)
		 */
		public void setInformationControl(IInformationControl control) {
			fInformationControlToClose= control;
		}

		/*
		 * @see IInformationControlCloser#start(Rectangle)
		 */
		public void start(Rectangle informationArea) {

			if (fIsActive)
				return;
			fIsActive= true;

			if (fSubjectControl != null && !fSubjectControl.isDisposed()) {
				fSubjectControl.addControlListener(this);
				fSubjectControl.addMouseListener(this);
				fSubjectControl.addFocusListener(this);
				fSubjectControl.addKeyListener(this);
			}

			if (fInformationControlToClose != null)
				fInformationControlToClose.addFocusListener(this);

			viwer.addViewportListener(this);
		}

		/*
		 * @see IInformationControlCloser#stop()
		 */
		public void stop() {

			if (!fIsActive)
				return;
			fIsActive= false;

			viwer.removeViewportListener(this);

			if (fInformationControlToClose != null)
				fInformationControlToClose.removeFocusListener(this);

			if (fSubjectControl != null && !fSubjectControl.isDisposed()) {
				fSubjectControl.removeControlListener(this);
				fSubjectControl.removeMouseListener(this);
				fSubjectControl.removeFocusListener(this);
				fSubjectControl.removeKeyListener(this);
			}
		}

		/*
		 * @see ControlListener#controlResized(ControlEvent)
		 */
		 public void controlResized(ControlEvent e) {
			 hideInformationControl();
		}

		/*
		 * @see ControlListener#controlMoved(ControlEvent)
		 */
		 public void controlMoved(ControlEvent e) {
			 hideInformationControl();
		}

		/*
		 * @see MouseListener#mouseDown(MouseEvent)
		 */
		 public void mouseDown(MouseEvent e) {
			 hideInformationControl();
		}

		/*
		 * @see MouseListener#mouseUp(MouseEvent)
		 */
		public void mouseUp(MouseEvent e) {
		}

		/*
		 * @see MouseListener#mouseDoubleClick(MouseEvent)
		 */
		public void mouseDoubleClick(MouseEvent e) {
			hideInformationControl();
		}

		/*
		 * @see FocusListener#focusGained(FocusEvent)
		 */
		public void focusGained(FocusEvent e) {
		}

		/*
		 * @see FocusListener#focusLost(FocusEvent)
		 */
		 public void focusLost(FocusEvent e) {
			Display d= fSubjectControl.getDisplay();
			d.asyncExec(new Runnable() {
				// Without the asyncExec, mouse clicks to the workbench window are swallowed.
				public void run() {
					if (fInformationControlToClose == null || !fInformationControlToClose.isFocusControl())
						hideInformationControl();
				}
			});
		}

		/*
		 * @see IViewportListenerListener#viewportChanged(int)
		 */
		public void viewportChanged(int topIndex) {
			hideInformationControl();
		}

		/*
		 * @see KeyListener#keyPressed(KeyEvent)
		 */
		public void keyPressed(KeyEvent e) {
			hideInformationControl();
		}

		/*
		 * @see KeyListener#keyReleased(KeyEvent)
		 */
		public void keyReleased(KeyEvent e) {
		}
	}


}
