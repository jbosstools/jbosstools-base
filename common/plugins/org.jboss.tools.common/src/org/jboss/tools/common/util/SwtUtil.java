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
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Resource;
import org.eclipse.swt.widgets.Shell;
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
	
	public static Shell getTopLevelShell(Shell shell) {
	  while (shell != null && shell.getParent() != null) {
	    shell = (Shell) shell.getParent();
	  }
	  return shell;
	}

  /**
   * Compute an optimum size based on the top level shell.
   * 
   * @param shell the shell to start from
   * @return the computed optimum size
   */
  public static Point getOptimumSizeFromTopLevelShell(Shell shell) {
    Rectangle bounds = getTopLevelShell(shell).getBounds();
    return new Point(bounds.width * 3 / 4, bounds.height * 3 / 4);
  }
}
