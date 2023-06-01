/*******************************************************************************
 * Copyright (c) 2020 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v2.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v20.html
 *
 * Contributors:
 * Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.ui.notification;

import org.eclipse.jface.notifications.AbstractNotificationPopup;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * @author Red Hat Developers
 *
 */
public class LabelNotification {
	private static class WidgetNotification extends AbstractNotificationPopup {

		private String text;

		public WidgetNotification(Shell shell, String text) {
			super(shell.getDisplay());
			this.text = text;
		}

		@Override
		protected void createContentArea(Composite parent) {
			Label label = new Label(parent, SWT.NONE);
			label.setText(text);
		}
	}

	private WidgetNotification window;

	public void close() {
		if (window != null) {
			window.close();
		}
	}

	public static LabelNotification openNotification(LabelNotification previous, Shell shell, String text) {
		LabelNotification notification = new LabelNotification();

		shell.getDisplay().asyncExec(() -> {
			if (previous != null) {
				previous.close();
			}
			notification.window = new WidgetNotification(shell, text);
			notification.window.open();
		});
		return notification;
	}

	public static LabelNotification openNotification(Shell shell, String text) {
		return openNotification(null, shell, text);
	}
}
