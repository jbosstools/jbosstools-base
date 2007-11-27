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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

/**
 * @author AU
 */
public class Viewport extends Composite {
	private ScrolledComposite scrolledComposite;

	public Viewport(Composite parent, int style) {
		super(parent, style);
		setBackground(parent.getBackground());
		scrolledComposite = (ScrolledComposite)parent;
		addListener(SWT.Resize, new Listener() {
			public void handleEvent(Event e) {
				if (e.type != SWT.Resize) return;
				resize();
			}
		});
	}
	
	private void resize() {
		scrolledComposite.layout();
	}

	public void layout(boolean changed) {
		scrolledComposite.layout();
		super.layout(changed);
	}

}
