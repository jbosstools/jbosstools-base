 /*******************************************************************************
  * Copyright (c) 2007-2009 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.ui.bot.ext.types;

/**
 * Support class for JobState evaluation
 * @author jpeterka
 */
public class JobState {
	public static final int NONE = 0;
	public static final int SLEEPING = 1;
	public static final int WAITING = 2;
	public static final int RUNNING = 4;

	public static String getStateName(int jobState) {
		String jobStateName = "";

		if (jobState == JobState.NONE)
			jobStateName = "None";
		else if (jobState == JobState.SLEEPING)
			jobStateName = "Sleeping";
		else if (jobState == 2)
			jobStateName = "Waiting";
		else if (jobState == 4)
			jobStateName = "Running";
		else
			throw new IllegalArgumentException("Unknown JobState");

		return jobStateName;
	}
}
