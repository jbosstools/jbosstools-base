package org.jboss.tools.ui.bot.ext.condition;

public enum TaskDuration {
	SHORT(10 * 1000), NORMAL(60 * 1000), LONG(5 * 60 * 1000), VERY_LONG(10 * 60 * 1000);
	
	private long timeout;
	
	private TaskDuration(long timeout) {
		this.timeout = timeout;
	}
	
	public long getTimeout() {
		return timeout;
	}
}