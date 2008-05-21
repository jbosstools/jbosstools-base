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
package org.jboss.tools.common.model.ui.editors.dnd;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Composite;

/**
 * @author Jeremy
 *
 * To change the template for this generated type comment go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class FreeCaretStyledText extends StyledText implements PaintListener {
    public FreeCaretStyledText(Composite parent, int style) {
        super(parent, style);
        super.addPaintListener(this);
    }
    
    public void dispose () {
        super.removePaintListener(this);
    }
    
    int fX = -1;
    int fY = -1;
    boolean fEnabled = false;
    
    public void enableFreeCaret(boolean set) {
        this.fEnabled = set;
        if (!set) redraw();
    }
   
    public void myRedraw(int x, int y) {
       this.fX = x;
       this.fY = y;

       if (this.fEnabled) {            
           redraw();
       }      
    }

    /* (non-Javadoc)
     * @see org.eclipse.swt.events.PaintListener#paintControl(org.eclipse.swt.events.PaintEvent)
     */
    public void paintControl(PaintEvent e) {
        if (!this.fEnabled) return;
        if (fX == -1 || fY == -1) return;
        GC gc = e.gc;
        gc.setForeground(gc.getBackground());
        int lh = getLineHeight();
        if (lh <= 0) lh = 3;
        gc.setXORMode(true);
        gc.setLineStyle(SWT.LINE_DOT);
        for (int i = 0; i < lh; i += 2) gc.drawLine(fX, fY + i, fX, fY + i);
    }
}
