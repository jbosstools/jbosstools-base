package org.jboss.tools.test.util.xpl;

import org.eclipse.core.runtime.jobs.Job;

public class DisplayDelayHelper extends DisplayHelper {
	
	private long currentTime;
	private long delay;

	public DisplayDelayHelper(long delay) {
		super();
		this.currentTime = System.currentTimeMillis();
		this.delay = delay;
	}

	@Override
	protected boolean condition() {
		return System.currentTimeMillis() > currentTime+delay;
	}

}
