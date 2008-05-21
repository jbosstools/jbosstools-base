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
package org.jboss.tools.common.verification.ui.vrules.wizard.runtime;

import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.verification.vrules.*;

public class VTaskListenerSafeImpl implements VTaskListener {
	VTaskListener listener = null;
	
	public VTaskListenerSafeImpl(VTaskListener listener) {
		this.listener = listener;
	}

	public void onStart() {
		Display.getDefault().syncExec( 
			new Runnable() {
				public void run() {
					listener.onStart();
				}
			}
		);
	}

	public void onRuleApplied(final VRule rule, final VObject object, final VResult[] results) {
		Display.getDefault().syncExec( 
			new Runnable() {
				public void run() {
					listener.onRuleApplied(rule, object, results);
				}
			}
		);
	}

	public void onRuleFinished(final VRule rule, final VObject object) {
		Display.getDefault().syncExec( 
			new Runnable() {
				public void run() {
					listener.onRuleFinished(rule, object);
				}
			}
		);
	}

	public void onPause() {
		Display.getDefault().syncExec( 
			new Runnable() {
				public void run() {
					listener.onPause();
				}
			}
		);
	}

	public void onResume() {
		Display.getDefault().syncExec( 
			new Runnable() {
				public void run() {
					listener.onResume();
				}
			}
		);
	}

	public void onFinish() {
		Display.getDefault().syncExec( 
			new Runnable() {
				public void run() {
					listener.onFinish();
				}
			}
		);
		
	}

}
