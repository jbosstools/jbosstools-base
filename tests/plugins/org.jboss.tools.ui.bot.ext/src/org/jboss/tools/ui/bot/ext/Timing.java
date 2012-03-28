package org.jboss.tools.ui.bot.ext;

import org.apache.log4j.Logger;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator;

/**
 * This class abstracts waiting time for bot. These methods should be used everywhere, where we 
 * are waiting for something. Time can be then globally adjusted 
 * using @link {@link TestConfigurator#SWTBOTEXT_DELAY_MULTIPLIER} system property
 * @author lzoubek
 *
 */
public class Timing {

	private static final Logger log = Logger.getLogger(Timing.class);
	
	public static double multiplier=1.0;
	
	static {
		try {
			multiplier = Double.parseDouble(System.getProperty(TestConfigurator.SWTBOTEXT_DELAY_MULTIPLIER, "1.0"));
			if (multiplier<0) {
				multiplier=1.0;
			}
		} catch (Exception ex) {
			log.error("Unable to parse TestConfigurator.SWTBOTEXT_DELAY_MULTIPLIER", ex);
		}
	}
	public static int time3S() {
		return  (int) Math.round(3000*multiplier);
	}
	public static int time2S() {
		return  (int) Math.round(2000*multiplier);
	}
	public static int time10S() {
		return (int) Math.round(10000*multiplier);
	}
	public static int time20S() {
		return (int) Math.round(20000*multiplier);
	}
	public static int time30S() {
		return (int) Math.round(30000*multiplier);
	}
	public static int time60S() {
		return (int) Math.round(60000*multiplier);
	}
	public static int timeUnlimited() {
		return (int) Math.round(999999999*multiplier);
	}	
	public static int time5S() {
		return (int) Math.round(5000*multiplier);
	}
	public static int time500MS() {
		return (int) Math.round(500*multiplier);
	}
	public static int time1S() {
		return (int) Math.round(1000*multiplier);
	}
	public static int time(int milis) {
		return (int) Math.round(milis*multiplier);
	}
	public static long time100S() {
		return (int) Math.round(100*1000*multiplier);
	}
}
