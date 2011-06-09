package org.jboss.tools.ui.bot.ext;
/**
 * an exception thrown when it's nopt possible to select required text within Source pane of VPE
 * @author vpakan
 *
 */
public class SelectTextInSourcePaneException extends RuntimeException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public SelectTextInSourcePaneException(String message, Throwable t) {
		super(message,t);
	}
	public SelectTextInSourcePaneException(String message) {
		super(message);
	}

}
