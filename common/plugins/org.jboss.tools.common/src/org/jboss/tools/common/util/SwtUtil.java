/*******************************************************************************
 * Copyright (c) 2007-2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.util;

import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Resource;
import org.eclipse.swt.widgets.Widget;

/**
 * @author Yahor Radtsevich (yradtsevich)
 */
public class SwtUtil {
	
	/**
	 * Adds a dispose listener to {@code widget}
	 * which when invoked disposes given {@code resource}
	 * 
	 * @param resource resource to be disposed
	 * @param widget widget to which disposal bind disposal of {@code resource}
	 */
	public static void bindDisposal(final Resource resource, Widget widget) {
		widget.addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				resource.dispose();
			}
		});
	}
}
