package org.jboss.tools.ui.bot.ext.config.requirement;
/**
 * an excetion thrown when error occurs by fulfilling test requirements
 * @author lzoubek
 *
 */
public class RequirementNotFulfilledException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public RequirementNotFulfilledException(String message, Throwable t) {
		super(message,t);
	}
	public RequirementNotFulfilledException(String message) {
		super(message);
	}

}
